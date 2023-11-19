import Debug "mo:base/Debug";
import Principal "mo:base/Principal";
import R "mo:base/Result";

import TokenHandler "mo:mrr/TokenHandler";

import MultiMinter "minter";

actor class MultiMinterAPI(ledger : ?Principal, admin : ?Principal) = self {

  type AssetId = Nat;
  type VirtualAccountId = Nat;

  func unwrapUninit<T>(opt : ?T) : T {
    switch (opt) {
      case (?o) o;
      case (null) Debug.trap("Not initialized");
    };
  };

  stable let ledgerPrincipal = switch (ledger) {
    case (?p) p;
    case (_) Debug.trap("Ledger not supplied");
  };

  stable var adminPrincipal = switch (admin) {
    case (?p) p;
    case (_) Debug.trap("Admin not supplied");
  };

  stable var stableData : MultiMinter.MultiMinterStableData = MultiMinter.defaultStableData();

  var minter : ?MultiMinter.MultiMinter = null;
  public shared func init() : async () {
    switch (minter) {
      case (null) minter := ?MultiMinter.MultiMinter(Principal.fromActor(self), ledgerPrincipal);
      case (_) Debug.trap("Already initialized");
    };
  };

  public query func tokenInfo(token : Text) : async R.Result<MultiMinter.SharedMintTokenData, ?{ #UnknownToken }> {
    switch (unwrapUninit(minter).tokenInfo(token)) {
      case (#ok info) #ok({
        symbol = info.symbol;
        assetId = info.assetId;
        icrc1Ledger = info.handler.icrc1LedgerPrincipal();
      });
      case (#err err) #err(?err);
    };
  };

  public query func assetId(token : Text) : async R.Result<AssetId, { #UnknownToken }> {
    R.mapOk<MultiMinter.MintTokenData, AssetId, { #UnknownToken }>(
      unwrapUninit(minter).tokenInfo(token),
      func(i) = i.assetId,
    );
  };

  public query func principalToSubaccount(p : Principal) : async ?Blob = async ?TokenHandler.toSubaccount(p);

  public query func usableBalance(token : Text, p : Principal) : async R.Result<Nat, { #UnknownToken }> {
    unwrapUninit(minter).balance(token, p);
  };

  public query func queryTokenHandlerJournal(token : Text, startFrom : ?Nat) : async R.Result<([TokenHandler.JournalRecord], Nat), ?{ #UnknownToken }> {
    switch (unwrapUninit(minter).tokenInfo(token)) {
      case (#ok info) #ok(info.handler.queryJournal(startFrom));
      case (#err err) #err(?err);
    };
  };

  public query func queryIcrc1Journal(startFrom : ?Nat) : async ([MultiMinter.JournalRecord], Nat) {
    unwrapUninit(minter).queryJournal(startFrom);
  };

  public query func stats() : async MultiMinter.Stats = async unwrapUninit(minter).stats();

  public query func isFrozen(token : Text) : async ?Bool = async unwrapUninit(minter).isFrozen(token);

  public shared ({ caller }) func registerFt(
    icrc1Ledger : Principal,
    symbol : Text,
    decimals : Nat8,
    description : Text,
  ) : async R.Result<MultiMinter.SharedMintTokenData, (MultiMinter.RegisterFtError or { #PermissionDenied })> {
    if (caller != adminPrincipal) {
      return #err(#PermissionDenied);
    };
    await* unwrapUninit(minter).registerFt(icrc1Ledger, symbol, decimals, description);
  };

  public shared func notify(token : Text, p : Principal) : async ?(Nat, Nat) {
    await* unwrapUninit(minter).notify(token, p);
  };

  public shared ({ caller }) func mint(
    token : Text,
    toAccount : (Principal, VirtualAccountId),
    mintAmount : { #amount : Nat; #max },
  ) : async R.Result<Nat, MultiMinter.MintError> {
    await* unwrapUninit(minter).mint(caller, token, toAccount, mintAmount);
  };

  public shared ({ caller }) func burnAndWithdraw(
    token : Text,
    burnAccount : VirtualAccountId,
    burnAmount : Nat,
    recipient : (Principal, ?TokenHandler.ICRC1.Subaccount),
  ) : async R.Result<Nat, MultiMinter.BurnError> {
    await* unwrapUninit(minter).burnAndWithdraw(token, (caller, burnAccount), burnAmount, recipient);
  };

  public shared ({ caller }) func withdrawCredit(
    token : Text,
    recipient : (Principal, ?TokenHandler.ICRC1.Subaccount),
  ) : async R.Result<Nat, MultiMinter.WithdrawCreditError> {
    await* unwrapUninit(minter).withdrawCredit(caller, token, recipient);
  };

  system func preupgrade() = switch (minter) {
    case (?m) {
      stableData := m.share();
    };
    case (_) {};
  };

  system func postupgrade() {
    minter := ?MultiMinter.MultiMinter(Principal.fromActor(self), ledgerPrincipal);
    // overwrite admin principal if provided. Note that we cannot overwrite ledger principal
    switch (admin) {
      case (?a) adminPrincipal := a;
      case (_) {};
    };
    switch (minter) {
      case (?m) m.unshare(stableData);
      case (_) Debug.trap("Could not unshare stable data");
    };
  };
};
