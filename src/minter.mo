import Principal "mo:base/Principal";
import R "mo:base/Result";
import RBTree "mo:base/RBTree";
import Text "mo:base/Text";
import AssocList "mo:base/AssocList";
import Time = "mo:base/Time";
import Timer "mo:base/Timer";
import Debug "mo:base/Debug";
import Option "mo:base/Option";
import Iter "mo:base/Iter";
import Int "mo:base/Int";
import Array "mo:base/Array";
import Nat "mo:base/Nat";

import Vec "mo:vector";
import TokenHandler "mo:mrr/TokenHandler";
import CircularBuffer "mo:mrr/CircularBuffer";

module {
  // The ledger types (can be imported in the future from public ledger library)
  type AssetId = Nat;
  type SubaccountId = Nat;
  type VirtualAccountId = Nat;
  type GlobalId = (Nat, Nat);
  type SubmitAndExecuteError = {
    #UnsupportedFeeMode : Text;
    //
    #IncorrectOwnerIndex;
    #TooLargeAssetId;
    #TooLargeFtQuantity;
    #TooLargeSubaccountId;
    #TooLargeVirtualAccountId;
    #UnsupportedMaxFlows;
    //
    #DeletedVirtualAccount;
    #InsufficientFunds;
    #MismatchInAsset; // asset in flow != asset in account
    #MismatchInRemotePrincipal; // remotePrincipal in flow != remotePrincipal in virtual account
    #NotAController; // in attempted mint operation
    #SuspendedVirtualAccount;
    #UnknownPrincipal;
    #UnknownSubaccount;
    #UnknownVirtualAccount;
    #UnknownFtAsset;
    //
    #NonZeroAssetSum;
    #TooLargeMemo;
    #TooManyFlows;
    #TooManyContributions;
    //
    #NotApproved;
  };
  type AccountReference = {
    #sub : SubaccountId;
    #vir : (Principal, VirtualAccountId);
    #mint;
  };

  public let JOURNAL_SIZE = 1024;

  public type LedgerInterface = actor {
    createFungibleToken : (decimals : Nat8, description : Text) -> async R.Result<AssetId, ?{ #NoSpace; #FeeError }>;
    simpleTransfer : (
      AccountReference,
      AccountReference,
      AssetId,
      { #amount : Nat; #max },
      { #senderPays; #receiverPays },
      ?Blob,
    ) -> async R.Result<(Nat, GlobalId), ?SubmitAndExecuteError>;
  };

  public type MintTokenData = {
    symbol : Text;
    assetId : AssetId;
    handler : TokenHandler.TokenHandler;
  };

  public type SharedMintTokenData = {
    symbol : Text;
    assetId : AssetId;
    icrc1Ledger : Principal;
  };

  public type MultiMinterStableData = [{
    tokenData : SharedMintTokenData;
    handlerData : TokenHandler.StableData;
  }];

  public func defaultStableData() : MultiMinterStableData = [];

  public type Icrc1TransferError = {
    #CallIcrc1LedgerError;
    #IcrcInsufficientFunds;
    #IcrcTemporarilyUnavailable;
    #IcrcGenericError;
  };

  public type RegisterFtError = {
    #AlreadyExists;
    #CallLedgerError;
    #FeeError;
    #NoSpace;
  };

  public type MintError = {
    #CallLedgerError;
    #DeletedVirtualAccount;
    #InsufficientBalance;
    #MismatchInAsset; // asset in flow != asset in account
    #MismatchInRemotePrincipal; // remotePrincipal in flow != remotePrincipal in virtual account
    #TooLargeFtQuantity;
    #TooLargeVirtualAccountId;
    #UnknownPrincipal;
    #UnknownToken;
    #UnknownVirtualAccount;
  };

  public type BurnError = Icrc1TransferError or {
    #CallLedgerError;
    #DeletedVirtualAccount;
    #InsufficientFunds;
    #MismatchInAsset; // asset in flow != asset in account
    #MismatchInRemotePrincipal; // remotePrincipal in flow != remotePrincipal in virtual account
    #TooLowQuantity;
    #TooLargeFtQuantity;
    #TooLargeVirtualAccountId;
    #UnknownPrincipal;
    #UnknownToken;
    #UnknownVirtualAccount;
  };

  public type WithdrawCreditError = { #NoCredit; #UnknownToken } or Icrc1TransferError;

  public type JournalRecord = (
    Time.Time,
    Principal,
    Text,
    {
      #mint : R.Result<(Nat, GlobalId), ?(SubmitAndExecuteError or { #CallLedgerError })>;
      #burn : R.Result<(Nat, GlobalId), ?(SubmitAndExecuteError or { #CallLedgerError })>;
      #burnWithdraw : R.Result<Nat, TokenHandler.ICRC1.TransferError or { #CallIcrc1LedgerError; #TooLowQuantity }>;
      #withdraw : R.Result<Nat, TokenHandler.ICRC1.TransferError or { #CallIcrc1LedgerError; #TooLowQuantity }>;
    },
  );

  public type Stats = {
    tokens : [{
      symbol : Text;
      backlogSize : Nat;
    }];
  };

  public class MultiMinter(
    ownPrincipal : Principal,
    ledgerPrincipal : Principal,
  ) {

    let ledger = actor (Principal.toText(ledgerPrincipal)) : LedgerInterface;

    var tokens : Vec.Vector<MintTokenData> = Vec.new<MintTokenData>();
    var tokenLookup : RBTree.RBTree<Text, Nat> = RBTree.RBTree<Text, Nat>(Text.compare);

    // A timer for consolidating backlog subaccounts
    let backlogTimer = Timer.recurringTimer(
      #seconds 10,
      func() : async () {
        for (asset in Vec.vals(tokens)) {
          await asset.handler.processBacklog();
        };
      },
    );

    // An event log
    var journal : CircularBuffer.CircularBuffer<JournalRecord> = CircularBuffer.CircularBuffer<JournalRecord>(JOURNAL_SIZE);

    func tokenInfo_(symbol : Text) : ?MintTokenData {
      switch (tokenLookup.get(symbol)) {
        case (?id) ?Vec.get(tokens, id);
        case (null) null;
      };
    };

    func tokenHandler_(symbol : Text) : ?TokenHandler.TokenHandler = Option.map<MintTokenData, TokenHandler.TokenHandler>(
      tokenInfo_(symbol),
      func(i) = i.handler,
    );

    public func tokenInfo(token : Text) : R.Result<MintTokenData, { #UnknownToken }> = R.fromOption(tokenInfo_(token), #UnknownToken);

    public func registerFt(icrc1Ledger : Principal, symbol : Text, decimals : Nat8, description : Text) : async* R.Result<SharedMintTokenData, RegisterFtError> {
      switch (tokenLookup.get(symbol)) {
        case (?index) return #err(#AlreadyExists);
        case (_) {};
      };
      let createFtResult = try {
        await ledger.createFungibleToken(decimals, description);
      } catch (err) {
        #err(? #CellLedgerError);
      };
      switch (createFtResult) {
        case (#err err) switch (err) {
          case (? #NoSpace) #err(#NoSpace);
          case (? #FeeError) #err(#FeeError);
          case (_) #err(#CallLedgerError);
        };
        case (#ok aid) {
          let index = Vec.size(tokens);
          let handler = TokenHandler.TokenHandler(icrc1Ledger, ownPrincipal, JOURNAL_SIZE);
          Vec.add(
            tokens,
            {
              symbol = symbol;
              assetId = aid;
              handler = handler;
            },
          );
          tokenLookup.put(symbol, index);
          #ok({
            symbol = symbol;
            assetId = aid;
            icrc1Ledger = icrc1Ledger;
          });
        };
      };
    };

    public func isFrozen(token : Text) : ?Bool = Option.map<TokenHandler.TokenHandler, Bool>(tokenHandler_(token), func(h) = h.isFrozen());

    public func notify(token : Text, p : Principal) : async* ?(Nat, Nat) {
      switch (tokenHandler_(token)) {
        case (?h) await* h.notify(p);
        case (null) null;
      };
    };

    public func balance(token : Text, p : Principal) : R.Result<Nat, { #UnknownToken }> {
      switch (tokenHandler_(token)) {
        case (?h) #ok(h.balance(p));
        case (_) #err(#UnknownToken);
      };
    };

    // query journal
    public func queryJournal(startFrom : ?Nat) : ([JournalRecord], Nat) = (
      Iter.toArray(
        journal.slice(
          Int.abs(Int.max(Option.get(startFrom, 0), journal.pushesAmount() - JOURNAL_SIZE)),
          journal.pushesAmount(),
        )
      ),
      journal.pushesAmount(),
    );

    public func mint(caller : Principal, token : Text, toAccount : (Principal, VirtualAccountId), mintAmount : { #amount : Nat; #max }) : async* R.Result<Nat, MintError> {
      let ?tokenInfo = tokenInfo_(token) else return #err(#UnknownToken);
      let handler = tokenInfo.handler;
      var balance = handler.balance(caller);
      let amount : Nat = switch (mintAmount) {
        case (#amount requested) {
          if (balance < requested) {
            switch (await* handler.notify(caller)) {
              case (?(_, usableBalance)) balance := usableBalance;
              case (null) {};
            };
          };
          requested;
        };
        case (#max) Nat.max(balance, 1);
      };
      if (balance < amount) {
        return #err(#InsufficientBalance);
      };
      if (not handler.debit(caller, amount)) {
        return #err(#InsufficientBalance);
      };
      let mintResult = try {
        await ledger.simpleTransfer(#mint, #vir(toAccount), tokenInfo.assetId, #amount(amount), #receiverPays, null);
      } catch (err) {
        #err(? #CallLedgerError);
      };
      journal.push((Time.now(), caller, token, #mint(mintResult)));
      switch (mintResult) {
        case (#ok _) #ok(amount);
        case (#err err) {
          ignore handler.credit(caller, amount);
          #err(castMintError(err));
        };
      };
    };

    public func burnAndWithdraw(token : Text, burnAccount : (Principal, VirtualAccountId), burnAmount : Nat, recipient : (Principal, ?TokenHandler.ICRC1.Subaccount)) : async* R.Result<Nat, BurnError> {
      let ?tokenInfo = tokenInfo_(token) else return #err(#UnknownToken);
      let handler = tokenInfo.handler;
      if (burnAmount <= handler.getFee()) { return #err(#TooLowQuantity) };
      let burnResult = try {
        await ledger.simpleTransfer(#vir(burnAccount), #mint, tokenInfo.assetId, #amount(burnAmount), #receiverPays, null);
      } catch (err) {
        #err(? #CallLedgerError);
      };
      journal.push((Time.now(), burnAccount.0, token, #burn(burnResult)));
      switch (burnResult) {
        case (#ok _) {};
        case (#err e) return #err(castBurnError(e));
      };
      let withdrawResult = await* handler.withdraw({ owner = recipient.0; subaccount = recipient.1 }, burnAmount);
      journal.push((Time.now(), burnAccount.0, token, #burnWithdraw(withdrawResult)));
      switch (withdrawResult) {
        case (#ok r) #ok(r);
        case (#err err) {
          ignore handler.credit(burnAccount.0, burnAmount);
          #err(castIcrc1TransferError(err));
        };
      };
    };

    public func withdrawCredit(caller : Principal, token : Text, recipient : (Principal, ?TokenHandler.ICRC1.Subaccount)) : async* R.Result<Nat, WithdrawCreditError> {
      let ?tokenInfo = tokenInfo_(token) else return #err(#UnknownToken);
      let handler = tokenInfo.handler;
      let credit = handler.info(caller).credit;
      if (credit <= handler.getFee() or not handler.debit(caller, Int.abs(credit))) {
        return #err(#NoCredit);
      };
      let withdrawResult = await* handler.withdraw({ owner = recipient.0; subaccount = recipient.1 }, Int.abs(credit));
      journal.push((Time.now(), caller, token, #withdraw(withdrawResult)));
      switch (withdrawResult) {
        case (#ok r) #ok(r);
        case (#err err) {
          ignore handler.credit(caller, Int.abs(credit));
          #err(castIcrc1TransferError(err));
        };
      };
    };

    public func stats() : Stats = {
      tokens = Iter.toArray(
        Iter.map<MintTokenData, { symbol : Text; backlogSize : Nat }>(
          Vec.vals(tokens),
          func(t) = {
            symbol = t.symbol;
            backlogSize = t.handler.backlogSize();
          },
        )
      );
    };

    private func castMintError(err : ?(SubmitAndExecuteError or { #CallLedgerError })) : MintError {
      switch (err) {
        case (?e) {
          switch (e) {
            case (#CallLedgerError) #CallLedgerError;
            case (#DeletedVirtualAccount) #DeletedVirtualAccount;
            case (#MismatchInAsset) #MismatchInAsset;
            case (#MismatchInRemotePrincipal) #MismatchInRemotePrincipal;
            case (#TooLargeFtQuantity) #TooLargeFtQuantity;
            case (#TooLargeVirtualAccountId) #TooLargeVirtualAccountId;
            case (#UnknownPrincipal) #UnknownPrincipal;
            case (#UnknownVirtualAccount) #UnknownVirtualAccount;
            case (_) Debug.trap("Unexpected error");
          };
        };
        case (null) Debug.trap("Unexpected error");
      };
    };

    private func castBurnError(err : ?(SubmitAndExecuteError or { #CallLedgerError })) : BurnError {
      switch (err) {
        case (?e) {
          switch (e) {
            case (#CallLedgerError) #CallLedgerError;
            case (#DeletedVirtualAccount) #DeletedVirtualAccount;
            case (#InsufficientFunds) #InsufficientFunds;
            case (#MismatchInAsset) #MismatchInAsset;
            case (#MismatchInRemotePrincipal) #MismatchInRemotePrincipal;
            case (#TooLargeFtQuantity) #TooLargeFtQuantity;
            case (#TooLargeVirtualAccountId) #TooLargeVirtualAccountId;
            case (#UnknownPrincipal) #UnknownPrincipal;
            case (#UnknownVirtualAccount) #UnknownVirtualAccount;
            case (_) Debug.trap("Unexpected error");
          };
        };
        case (null) Debug.trap("Unexpected error");
      };
    };

    private func castIcrc1TransferError(err : TokenHandler.ICRC1.TransferError or { #CallIcrc1LedgerError; #TooLowQuantity }) : Icrc1TransferError {
      switch (err) {
        case (#CallIcrc1LedgerError) #CallIcrc1LedgerError;
        case (#InsufficientFunds _) #IcrcInsufficientFunds;
        case (#TemporarilyUnavailable _) #IcrcTemporarilyUnavailable;
        case (#GenericError _) #IcrcGenericError;
        case (_) Debug.trap("Unexpected ICRC1 error");
      };
    };

    public func share() : MultiMinterStableData {
      let tokensAmount = Vec.size(tokens);
      Array.tabulate<{ tokenData : SharedMintTokenData; handlerData : TokenHandler.StableData }>(
        tokensAmount,
        func(n) {
          let info = Vec.get(tokens, n);
          {
            tokenData = {
              symbol = info.symbol;
              assetId = info.assetId;
              icrc1Ledger = info.handler.icrc1LedgerPrincipal();
            };
            handlerData = info.handler.share();
          };
        },
      );
    };

    public func unshare(value : MultiMinterStableData) {
      tokens := Vec.new<MintTokenData>();
      tokenLookup := RBTree.RBTree<Text, Nat>(Text.compare);
      var i = 0;
      for (info in Array.vals(value)) {
        let handler = TokenHandler.TokenHandler(info.tokenData.icrc1Ledger, ownPrincipal, JOURNAL_SIZE);
        handler.unshare(info.handlerData);
        Vec.add(
          tokens,
          {
            symbol = info.tokenData.symbol;
            assetId = info.tokenData.assetId;
            handler = handler;
          },
        );
        tokenLookup.put(info.tokenData.symbol, i);
        i += 1;
      };
    };

  };
};
