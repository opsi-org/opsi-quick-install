unit opsi_quick_install_nogui_query;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  opsiquickinstall_QueryData,
  opsi_quick_install_resourcestrings,
  DistributionInfo,
  osnetworkcalculator,
  osnetutil,
  SupportedOpsiServerDistributions,
  opsi_quick_install_nogui_NetworkSuggestions;

type
  TQueryProcedure = procedure of object;
  TQueryProceduresList = array of TQueryProcedure;

  TQuickInstallNoQuiQuery = class(TObject)
  private
    input: string;
    NetworkDetails: array of string;
    FJumpToOverviewAllowed: boolean;

    function CheckJumpToOverview: boolean;
    procedure CheckInput(ValidOptions: string; EmptyInputAllowed: boolean;
      HelpInfo: string);
    procedure CheckHelp(HelpInfo: string);
    function JumpBackToQuery(QueryProcedure: TQueryProcedure): boolean;

    procedure QueryDistribution;
    procedure QuerySetupType;
    //procedure QueryOpsiVersion;
    procedure QueryRepo;
    procedure QueryProxy;
    procedure QueryRepoNoCache;
    procedure QueryBackend;
    procedure QueryCopyModules;
    procedure QueryRepoKind;
    procedure QueryUCSPassword;
    procedure QueryReboot;
    procedure QueryDhcp;
    procedure QueryLink;
    procedure QueryNetmask;
    procedure QueryNetworkAddress;
    procedure QueryDomain;
    procedure QueryNameserver;
    procedure QueryGateway;
    procedure QueryAdminName;
    procedure QueryAdminPassword;
    procedure QueryIPName;
    procedure QueryIPNumber;
    procedure JumpBackFromOverviewToQuery(QueryProcedure: TQueryProcedure);
    procedure PrintOverview;
    function GetAskedQueries: TQueryProceduresList;
    procedure QueryOverview;
  public
    QueryFinished: boolean;
    procedure StartQuery;
  end;



implementation

function TQuickInstallNoQuiQuery.CheckJumpToOverview: boolean;
begin
  Result := False;
  if (input = '-o') and FJumpToOverviewAllowed then
  begin
    Result := True;
    QueryOverview;
  end;
end;

procedure TQuickInstallNoQuiQuery.CheckInput(ValidOptions: string;
  EmptyInputAllowed: boolean; HelpInfo: string);
var
  ListOfValidOptions: TStringList;
begin
  ListOfValidOptions := TStringList.Create;
  ListOfValidOptions.AddCommaText(ValidOptions);
  if EmptyInputAllowed then ListOfValidOptions.add('');

  while (ListOfValidOptions.IndexOf(input) = -1) do
  begin
    if (input = '-h') and (HelpInfo <> '') then
      writeln(HelpInfo)
    else
      writeln('"', input, '"', rsNotValid);

    readln(input);
  end;
end;

procedure TQuickInstallNoQuiQuery.CheckHelp(HelpInfo: string);
begin
  readln(input);
  while input = '-h' do
  begin
    writeln(HelpInfo);
    readln(input);
  end;
end;

function TQuickInstallNoQuiQuery.JumpBackToQuery(QueryProcedure:
  TQueryProcedure): boolean;
begin
  Result := (input = '-b');
  if Result then QueryProcedure;
end;


// Input variables not set by resourcestrings but by characters for no
// requirement of a mouse.

procedure TQuickInstallNoQuiQuery.StartQuery;
begin
  FJumpToOverviewAllowed := False;
  Data.adminPassword := '';
  QueryDistribution;
end;

procedure TQuickInstallNoQuiQuery.QueryDistribution;
var
  UserEditedDistroName, UserEditedDistroRelease: string;
begin
  writeln(rsDistr, ' ', Data.DistrInfo.DistroName, ' ', Data.DistrInfo.DistroRelease);
  writeln(rsIsCorrect, rsYesNoOp, '*');
  readln(input);
  CheckInput('y,n', True, rsInfoDistribution + #10 + SupportedDistributionsInfoString);

  // if distribution isn't correct, read the correct one
  if input = 'n' then
  begin
    writeln(rsOtherDistr);
    readln(input);
    UserEditedDistroName := Copy(input, 1, Pos(' ', input) - 1);
    UserEditedDistroRelease :=
      Copy(input, Pos(' ', input) + 1, Length(input) - Pos(' ', input));
    Data.DistrInfo.CorrectDistribution(UserEditedDistroName,
      UserEditedDistroRelease);
  end;

  if Data.DistrInfo.Distr = other then
  begin
    writeln('');
    writeln(rsNoSupport + #10 + SupportedDistributionsInfoString);
    Exit;
  end;
  QuerySetupType;
end;

procedure TQuickInstallNoQuiQuery.QuerySetupType;
begin
  writeln(rsSetup, rsSetupOp);
  readln(input);
  CheckInput('s,c,-b', True, '');

  // if input = -b then go back to the previous query
  if not JumpBackToQuery(@QueryDistribution) then
  begin
    case input of
      'c': Data.CustomSetup := True;
      's': Data.CustomSetup := False;
      '': Data.CustomSetup := False;
    end;

    if Data.CustomSetup then
      // following queries only for custom setup
      //QueryOpsiVersion
      QueryRepo
    else
    if lowerCase(Data.DistrInfo.DistroName) = 'univention' then
      QueryUCSPassword
    else
      QueryDhcp;
  end;
end;

{procedure TQuickInstall.QueryOpsiVersion;
begin
  // opsi version:
  writeln(rsOpsiVersion, rsOpsiVersionOp, '*');
  readln(input);
  while not ((input = '1') or (input = '2') or (input = '-b') or
      (input = '')) do
  begin
    if input = '-h' then
      writeln(rsInfoOpsiVersion)
    else
      writeln('"', input, '"', rsNotValid);
    readln(input);
  end;
  if input = '-b' then
    QuerySetupType
  else
  begin
    opsiVersion := 'Opsi 4.' + input;
    if input = '' then
      opsiVersion := 'Opsi 4.1';
    QueryRepo;
  end;
end;}

procedure TQuickInstallNoQuiQuery.QueryRepo;
begin
  if Data.opsiVersion = 'Opsi 4.1' then
    writeln(rsRepo, ' [Example: ', Data.baseRepoUrlOpsi41, ']', '*')
  else if Data.opsiVersion = 'Opsi 4.2' then
    writeln(rsRepo, ' [Example: ', Data.baseRepoUrlOpsi42, ']', '*');

  readln(input);
  if CheckJumpToOverview then Exit;
  while not ((Pos('http', input) = 1) or (input = '-b') or (input = '')) do
  begin
    if input = '-h' then
      writeln(rsInfoRepo)
    else
      writeln('"', input, '"', rsNotValid);
    readln(input);
  end;

  if not JumpBackToQuery(@QuerySetupType) then
  begin // cases input = 'http...', input = ''
    if input = '' then
    begin
      case Data.opsiVersion of
        'Opsi 4.1': Data.repo := Data.baseRepoUrlOpsi41;
        'Opsi 4.2': Data.repo := Data.baseRepoUrlOpsi42;
      end
    end
    else
      Data.repo := input;

    QueryProxy;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryProxy;
begin
  writeln(rsUseProxy, rsYesNoOp);
  readln(input);
  if CheckJumpToOverview then Exit;
  CheckInput('y,n,-b', True, '');

  if not JumpBackToQuery(@QueryRepo) then
  begin
    case input of
      '': Data.proxy := '';
      'n': Data.proxy := '';
      'y':
      begin
        writeln('Which Proxy would you like to use? [Example: "http://myproxy.dom.org:8080"]');
        readln(input);
        Data.proxy := input;
      end;
    end;
    QueryRepoNoCache;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryRepoNoCache;
begin
  // repo without cache proxy:
  if Data.opsiVersion = 'Opsi 4.1' then
    writeln(rsRepoNoCache, ' [Example: ', Data.baseRepoUrlOpsi41, ']')
  else if Data.opsiVersion = 'Opsi 4.2' then
    writeln(rsRepoNoCache, ' [Example: ', Data.baseRepoUrlOpsi42, ']');

  readln(input);
  if CheckJumpToOverview then Exit;
  while not ((Pos('http', input) = 1) or (input = '-b') or (input = '')) do
  begin
    writeln('"', input, '"', rsNotValid);
    readln(input);
  end;

  if not JumpBackToQuery(@QueryProxy) then
  begin // cases input = 'http...', input = ''
    if input = '' then
      Data.repoNoCache := Data.repo
    else
      Data.repoNoCache := input;

    QueryBackend;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryBackend;
begin
  writeln(rsBackend, rsBackendOp, '*');
  readln(input);
  if CheckJumpToOverview then Exit;
  CheckInput('f,m,-b', True, rsInfoBackend);

  if not JumpBackToQuery(@QueryRepoNoCache) then
  begin
    case input of
      'm': Data.backend := 'mysql';
      'f': Data.backend := 'file';
      '': Data.backend := 'file';
    end;

    if Data.backend = 'mysql' then
      QueryCopyModules
    else
      QueryRepoKind;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryCopyModules;
begin
  writeln(rsCopyModules, rsYesNoOp, '*');
  readln(input);
  if CheckJumpToOverview then Exit;
  CheckInput('y,n,-b', True, rsInfoModules);

  if not JumpBackToQuery(@QueryBackend) then
  begin
    case input of
      'y': Data.copyMod.SetEntries('true');
      'n': Data.copyMod.SetEntries('false');
      '': Data.copyMod.SetEntries('false');
    end;
    QueryRepoKind;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryRepoKind;
begin
  writeln(rsRepoKind, rsRepoKindOp, '*');
  readln(input);
  if CheckJumpToOverview then Exit;
  CheckInput('e,t,s,-b', True, rsInfoRepoKind);

  if input = '-b' then
  begin
    if Data.backend = 'mysql' then
      QueryCopyModules
    else
      QueryBackend;
  end
  else
  begin
    case input of
      'e': Data.repoKind := 'experimental';
      't': Data.repoKind := 'testing';
      's': Data.repoKind := 'stable';
      '': Data.repoKind := 'stable';
    end;

    if lowerCase(Data.DistrInfo.DistroName) = 'univention' then
      QueryUCSPassword
    else
      QueryReboot;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryUCSPassword;
begin
  writeln(rsUCS);
  readln(input);
  if CheckJumpToOverview then Exit;
  if input = '-b' then // go back
  begin
    if not Data.CustomSetup then
      QuerySetupType
    else
      QueryRepoKind;
  end
  else // go forward
  begin
    Data.ucsPassword := input;
    if Data.CustomSetup then
      QueryReboot
    else
      QueryDhcp;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryReboot;
begin
  writeln(rsReboot, rsYesNoOp, '*');
  readln(input);
  if CheckJumpToOverview then Exit;
  CheckInput('y,n,-b', True, rsInfoReboot);

  if input = '-b' then
  begin
    if lowerCase(Data.DistrInfo.DistroName) = 'univention' then
      QueryUCSPassword
    else
      QueryRepoKind;
  end
  else
  begin
    case input of
      'y': Data.reboot.SetEntries('true');
      'n': Data.reboot.SetEntries('false');
      '': Data.reboot.SetEntries('false');
    end;
    QueryDhcp;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryDhcp;
begin
  writeln(rsDhcp, rsYesNoOp, '*');
  readln(input);
  if CheckJumpToOverview then Exit;
  CheckInput('y,n,-b', True, rsInfoDhcp);

  if input = '-b' then
  begin
    if Data.CustomSetup then
      QueryReboot
    else
    begin
      if lowerCase(Data.DistrInfo.DistroName) = 'univention' then
        QueryUCSPassword
      else
        QuerySetupType;
    end;
  end
  else
  begin
    case input of
      'y': Data.dhcp.SetEntries('true');
      'n': Data.dhcp.SetEntries('false');
      '': Data.dhcp.SetEntries('false');
    end;

    if Data.dhcp.PropertyEntry = 'true' then
    begin
      // read in network details for dhcp queries
      NetworkDetails := getNetworkDetails(['IP4.ADDRESS[1]',
        'IP4.ADDRESS[2]', 'IP4.ADDRESS[3]', 'IP4.DOMAIN[1]',
        'IP4.DOMAIN[2]', 'IP4.DOMAIN[3]', 'IP4.DNS[1]', 'IP4.DNS[2]',
        'IP4.DNS[3]', 'IP4.GATEWAY']);
      // following queries only for dhcp
      QueryLink;
    end
    else
      QueryAdminName;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryLink;
begin
  writeln(rsTFTPROOT, rsLinkOp, '*');
  readln(input);
  if CheckJumpToOverview then Exit;
  CheckInput('m,nom,-b', True, rsInfoTFTPROOT);

  if not JumpBackToQuery(@QueryDhcp) then
  begin
    case input of
      'm': Data.symlink := 'default.menu';
      'nom': Data.symlink := 'default.nomenu';
      '': Data.symlink := 'default.nomenu';
    end;
    QueryNetmask;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryNetmask;
var
  Suggestions: string;
begin
  Suggestions := GetNetmaskSuggestions(NetworkDetails);
  writeln(rsNetmask, rsSuggestion, Suggestions, ']*');
  CheckHelp(rsInfoNetwork);
  if CheckJumpToOverview then Exit;

  if not JumpBackToQuery(@QueryLink) then
  begin
    if input = '' then
      Data.netmask := TakeFirstSuggestion(Suggestions)
    else
      Data.netmask := input;
    QueryNetworkAddress;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryNetworkAddress;
var
  Suggestions: string;
begin
  Suggestions := GetNetworkAddressSuggestions(NetworkDetails);
  writeln(rsNetworkAddress, rsSuggestion, Suggestions, ']*');
  CheckHelp(rsInfoNetwork);
  if CheckJumpToOverview then Exit;

  if not JumpBackToQuery(@QueryNetmask) then
  begin
    if input = '' then
      Data.networkAddress := TakeFirstSuggestion(Suggestions)
    else
      Data.networkAddress := input;
    QueryDomain;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryDomain;
var
  Suggestions: string;
begin
  Suggestions := GetDomainSuggestions(NetworkDetails);
  writeln(rsDomain, rsSuggestion, Suggestions, ']*');
  CheckHelp(rsInfoNetwork);
  if CheckJumpToOverview then Exit;

  if not JumpBackToQuery(@QueryNetworkAddress) then
  begin
    if input = '' then
      Data.domain := TakeFirstSuggestion(Suggestions)
    else
      Data.domain := input;
    QueryNameserver;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryNameserver;
var
  Suggestions: string;
begin
  Suggestions := GetNameserverSuggestions(NetworkDetails);
  writeln(rsNameserver, rsSuggestion, Suggestions, ']*');
  CheckHelp(rsInfoNetwork);
  if CheckJumpToOverview then Exit;

  if not JumpBackToQuery(@QueryDomain) then
  begin
    if input = '' then
      Data.nameserver := TakeFirstSuggestion(Suggestions)
    else
      Data.nameserver := input;
    QueryGateway;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryGateway;
var
  suggestion: string = '';
begin
  // IP4.GATEWAY
  if NetworkDetails[9] <> '' then
    suggestion := NetworkDetails[9];

  writeln(rsGateway, rsSuggestion, suggestion, ']*');
  CheckHelp(rsInfoNetwork);
  if CheckJumpToOverview then Exit;

  if not JumpBackToQuery(@QueryNameserver) then
  begin
    if input = '' then
      Data.gateway := suggestion
    else
      Data.gateway := input;
    QueryAdminName;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryAdminName;
begin
  writeln(rsAdminName, '*');
  CheckHelp(rsInfoAdmin);
  if CheckJumpToOverview then Exit;

  if input = '-b' then
  begin
    if Data.dhcp.PropertyEntry = 'true' then
      QueryGateway
    else
      QueryDhcp;
  end
  else
  begin
    Data.adminName := input;
    if input = '' then
      QueryIPName
    else
      QueryAdminPassword;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryAdminPassword;
begin
  writeln(rsAdminPassword, '*');
  readln(input);
  if CheckJumpToOverview then Exit;
  if not JumpBackToQuery(@QueryAdminName) then
  begin
    Data.adminPassword := input;
    QueryIPName;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryIPName;
begin
  writeln(rsIPName);
  readln(input);
  if CheckJumpToOverview then Exit;
  while not ((input = 'auto') or isValidFQDN(input) or (input = '-b') or (input = '')) do
  begin
    writeln('"', input, '"', rsNotValid);
    readln(input);
  end;

  if input = '-b' then
  begin
    if Data.adminName = '' then
      QueryAdminName
    else
      QueryAdminPassword;
  end
  else
  begin
    if input = '' then
      Data.ipName := 'auto'
    else
      Data.ipName := input;

    QueryIPNumber;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryIPNumber;
begin
  writeln(rsIPNumber);
  readln(input);
  if CheckJumpToOverview then Exit;
  while not ((input = 'auto') or isValidIP4(input) or (input = '-b') or (input = '')) do
  begin
    writeln('"', input, '"', rsNotValid);
    readln(input);
  end;

  if not JumpBackToQuery(@QueryIPName) then
  begin
    if input = '' then
      Data.ipNumber := 'auto'
    else
      Data.ipNumber := input;

    QueryOverview;
  end;
end;

procedure TQuickInstallNoQuiQuery.JumpBackFromOverviewToQuery(
  QueryProcedure: TQueryProcedure);
begin
  QueryProcedure;
end;

procedure TQuickInstallNoQuiQuery.PrintOverview;
var
  // number of asked question
  Counter: integer = 1;
begin
  writeln('');
  writeln(rsOverview);
  writeln(rsOpsiVersionO, Data.opsiVersion);
  {Custom installation}
  if Data.CustomSetup then
  begin
    writeln(Counter, ' ', rsRepoO, Data.repo);
    Inc(Counter);
    writeln(Counter, ' ', rsProxyO, Data.proxy);
    Inc(Counter);
    writeln(Counter, ' ', rsRepoNoCacheO, Data.repoNoCache);
    Inc(Counter);
    writeln(Counter, ' ', rsBackendO, Data.backend);
    Inc(Counter);
    if Data.backend = 'mysql' then
    begin
      writeln(Counter, ' ', rsCopyModulesO, Data.copyMod.OverviewEntry);
      Inc(Counter);
    end;
    writeln(Counter, ' ', rsRepoKindO, Data.repoKind);
    Inc(Counter);
  end;
  {Both}
  if lowerCase(Data.DistrInfo.DistroName) = 'univention' then
  begin
    writeln(Counter, ' ', rsUCSO, Data.ucsPassword);
    Inc(Counter);
  end;
  {Custom installation}
  if Data.CustomSetup then
  begin
    writeln(Counter, ' ', rsRebootO, Data.reboot.OverviewEntry);
    Inc(Counter);
  end;
  {Both}
  writeln(Counter, ' ', rsDhcpO, Data.dhcp.OverviewEntry);
  Inc(Counter);
  if Data.dhcp.PropertyEntry = 'true' then
  begin
    writeln(Counter, ' ', rsTFTPROOTO, Data.symlink);
    Inc(Counter);
    writeln(Counter, ' ', rsNetmaskO, Data.netmask);
    Inc(Counter);
    writeln(Counter, ' ', rsNetworkO, Data.networkAddress);
    Inc(Counter);
    writeln(Counter, ' ', rsDomainO, Data.domain);
    Inc(Counter);
    writeln(Counter, ' ', rsNameserverO, Data.nameserver);
    Inc(Counter);
    writeln(Counter, ' ', rsGatewayO, Data.gateway);
    Inc(Counter);
  end;
  writeln(Counter, ' ', rsAdminNameO, Data.adminName);
  Inc(Counter);
  if Data.adminName <> '' then
  begin
    writeln(Counter, ' ', rsAdminPasswordO, Data.adminPassword);
    Inc(Counter);
  end;
  writeln(Counter, ' ', rsIPNameO, Data.ipName);
  Inc(Counter);
  writeln(Counter, ' ', rsIPNumberO, Data.ipNumber);
end;

procedure AddQueryToList(QueryProcedure: TQueryProcedure;
  var QueryProceduresList: TQueryProceduresList);
begin
  SetLength(QueryProceduresList, Length(QueryProceduresList) + 1);
  QueryProceduresList[Length(QueryProceduresList) - 1] := QueryProcedure;
end;

function TQuickInstallNoQuiQuery.GetAskedQueries: TQueryProceduresList;
var
  QueryProceduresList: TQueryProceduresList;
begin
  // find the questions that were asked (depending on setup type and
  // distribution=Univention) and return their names
  //Result := TStringList.Create;
  QueryProceduresList := TQueryProceduresList.Create;
  SetLength(QueryProceduresList, 0);

  {Custom installation}
  if Data.CustomSetup then
  begin
    AddQueryToList(@QueryRepo, QueryProceduresList);
    AddQueryToList(@QueryProxy, QueryProceduresList);
    AddQueryToList(@QueryRepoNoCache, QueryProceduresList);
    AddQueryToList(@QueryBackend, QueryProceduresList);

    if Data.backend = 'mysql' then
      AddQueryToList(@QueryCopyModules, QueryProceduresList);

    AddQueryToList(@QueryRepoKind, QueryProceduresList);
  end;

  {Both}
  if lowerCase(Data.DistrInfo.DistroName) = 'univention' then
    AddQueryToList(@QueryUCSPassword, QueryProceduresList);

  {Custom installation}
  if Data.CustomSetup then
    AddQueryToList(@QueryReboot, QueryProceduresList);

  {Both}
  AddQueryToList(@QueryDhcp, QueryProceduresList);
  if Data.dhcp.PropertyEntry = 'true' then
  begin
    AddQueryToList(@QueryLink, QueryProceduresList);
    AddQueryToList(@QueryNetmask, QueryProceduresList);
    AddQueryToList(@QueryNetworkAddress, QueryProceduresList);
    AddQueryToList(@QueryDomain, QueryProceduresList);
    AddQueryToList(@QueryNameserver, QueryProceduresList);
    AddQueryToList(@QueryGateway, QueryProceduresList);
  end;

  AddQueryToList(@QueryAdminName, QueryProceduresList);
  if Data.adminName <> '' then
    AddQueryToList(@QueryAdminPassword, QueryProceduresList);

  AddQueryToList(@QueryIPName, QueryProceduresList);
  AddQueryToList(@QueryIPNumber, QueryProceduresList);
  //writeln(Result.Text);

  Result := QueryProceduresList;
end;

procedure TQuickInstallNoQuiQuery.QueryOverview;
var
  // list of the asked questions by numbers
  QueryProceduresList: TQueryProceduresList;
  QueryIndex: integer;
  ValidQueryIndex: boolean = False;
begin
  PrintOverview;
  QueryProceduresList := GetAskedQueries;

  writeln('');
  writeln(rsContinue);
  // Jumping back to a query by the number in the overview:
  readln(input);
  try
    QueryIndex := StrToInt(input) - 1;
    if (QueryIndex >= 0) and (QueryIndex < Length(QueryProceduresList)) then
      ValidQueryIndex := True;
  except
  end;

  // only elements of 'queries' (jumping back) or '' (start installation) are valid inputs
  while not (ValidQueryIndex or (input = '')) do
  begin
    ValidQueryIndex := False;
    writeln('"', input, '"', rsNotValid);
    readln(input);
    try
      QueryIndex := StrToInt(input) - 1;
      if (QueryIndex >= 0) and (QueryIndex < Length(QueryProceduresList)) then
        ValidQueryIndex := True;
    except
    end;
  end;
  // jump back to the respective question
  if input <> '' then
  begin
    FJumpToOverviewAllowed := True;
    JumpBackFromOverviewToQuery(QueryProceduresList[QueryIndex]);
  end
  else
    QueryFinished := True;
end;


end.
