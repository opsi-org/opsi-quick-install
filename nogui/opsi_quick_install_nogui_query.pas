unit opsi_quick_install_nogui_query;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  opsiquickinstall_data,
  opsi_quick_install_resourcestrings,
  DistributionInfo,
  osnetworkcalculator,
  osnetutil,
  SupportedOpsiServerDistributions;

type
  TQueryProcedure = procedure of object;
  TQueryProceduresList = array of TQueryProcedure;

  TQuickInstallNoQuiQuery = class(TObject)
  private
    input: string;
    NetworkDetails: array of string;

    procedure CheckInput(ValidOptions: string; EmptyInputAllowed: boolean;
      HelpInfo: string);
    procedure CheckHelp(HelpInfo: string);
    procedure QueryDistribution;
    procedure QuerySetupType;
    //procedure QueryOpsiVersion;
    procedure QueryRepo;
    procedure QueryProxy;
    procedure QueryRepoNoCache;
    procedure QueryBackend;
    procedure QueryModules;
    procedure QueryRepoKind;
    procedure QueryUCS;
    procedure QueryReboot;
    procedure QueryDhcp;
    procedure QueryLink;
    function GetNetmaskSuggestions: string;
    procedure QueryNetmask;
    function GetNetworkAddressSuggestions: string;
    procedure QueryNetworkAddress;
    function GetDomainSuggestions: string;
    procedure QueryDomain;
    function GetNameserverSuggestions: string;
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

procedure TQuickInstallNoQuiQuery.CheckInput(ValidOptions: string;
  EmptyInputAllowed: boolean; HelpInfo: string);
var
  ListOfValidOptions: TStringList;
begin
  ListOfValidOptions := TStringList.Create;
  ListOfValidOptions.AddCommaText(ValidOptions);
  if EmptyInputAllowed then ListOfValidOptions.add('');

  readln(input);
  while (ListOfValidOptions.IndexOf(input) = -1) do
  begin
    if (input = '-h') and (HelpInfo <> '') then
      writeln(HelpInfo)
    else
      writeln('"', input, '"', rsNotValid);

    readln(input);
  end;

  if Assigned(ListOfValidOptions) then FreeAndNil(ListOfValidOptions);
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


// Input variables not set by resourcestrings but by characters for no
// requirement of a mouse.

procedure TQuickInstallNoQuiQuery.StartQuery;
begin
  QueryDistribution;
end;

procedure TQuickInstallNoQuiQuery.QueryDistribution;
var
  UserEditedDistroName, UserEditedDistroRelease: string;
begin
  writeln(rsDistr, ' ', Data.DistrInfo.DistroName, ' ', Data.DistrInfo.DistroRelease);
  writeln(rsIsCorrect, rsYesNoOp, '*');
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
  CheckInput('s,c,-b', True, '');

  // if input = -b then go back to the previous query
  if input = '-b' then
    QueryDistribution
  else
  begin
    if input = 'c' then
      Data.CustomSetup := True
    else // cases input = 's', input = ''
      Data.CustomSetup := False;

    if Data.CustomSetup then
      // following queries only for custom setup
      //QueryOpsiVersion
      QueryRepo
    else
    if lowerCase(Data.DistrInfo.DistroName) = 'univention' then
      QueryUCS
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
  while not ((Pos('http', input) = 1) or (input = '-b') or (input = '')) do
  begin
    if input = '-h' then
      writeln(rsInfoRepo)
    else
      writeln('"', input, '"', rsNotValid);
    readln(input);
  end;

  if input = '-b' then
    //QueryOpsiVersion
    QuerySetupType
  else
  begin // cases input = 'http...', input = ''
    if input = '' then
    begin
      if Data.opsiVersion = 'Opsi 4.1' then
        Data.repo := Data.baseRepoUrlOpsi41
      else
      if Data.opsiVersion = 'Opsi 4.2' then
        Data.repo := Data.baseRepoUrlOpsi42;
    end
    else
    begin
      Data.repo := input;
    end;

    QueryProxy;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryProxy;
begin
  writeln(rsUseProxy, rsYesNoOp);
  CheckInput('y,n,-b', True, '');

  if input = '-b' then
    QueryRepo
  else
  begin
    if input = 'y' then
    begin
      writeln('Which Proxy would you like to use? [Example: "http://myproxy.dom.org:8080"]');
      readln(input);
      Data.proxy := input;
    end
    else // cases input = 'n', input = ''
      Data.proxy := '';

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
  while not ((Pos('http', input) = 1) or (input = '-b') or (input = '')) do
  begin
    writeln('"', input, '"', rsNotValid);
    readln(input);
  end;

  if input = '-b' then
    QueryProxy
  else
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
  CheckInput('f,m,-b', True, rsInfoBackend);

  if input = '-b' then
    QueryRepoNoCache
  else
  begin
    if input = 'm' then
      Data.backend := 'mysql'
    else
      Data.backend := 'file'; // cases input = 'f', input = ''

    if Data.backend = 'mysql' then
      QueryModules
    else
      QueryRepoKind;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryModules;
begin
  // copy modules:
  writeln(rsCopyModules, rsYesNoOp, '*');
  CheckInput('y,n,-b', True, rsInfoModules);

  if input = '-b' then
    QueryBackend
  else
  begin
    if input = 'y' then
      Data.copyMod.SetEntries(rsYes, 'true')
    else
      Data.copyMod.SetEntries(rsNo, 'false'); // cases input = 'n', input = ''

    QueryRepoKind;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryRepoKind;
begin
  writeln(rsRepoKind, rsRepoKindOp, '*');
  CheckInput('e,t,s,-b', True, rsInfoRepoKind);

  if input = '-b' then
  begin
    if Data.backend = 'mysql' then
      QueryModules
    else
      QueryBackend;
  end
  else
  begin
    if input = 'e' then
      Data.repoKind := 'experimental'
    else if input = 't' then
      Data.repoKind := 'testing'
    else
      Data.repoKind := 'stable'; // cases input = 's', input = ''

    if lowerCase(Data.DistrInfo.DistroName) = 'univention' then
      QueryUCS
    else
      QueryReboot;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryUCS;
begin
  // ucs password:
  writeln(rsUCS);
  readln(input);
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
  CheckInput('y,n,-b', True, rsInfoReboot);

  if input = '-b' then
  begin
    if lowerCase(Data.DistrInfo.DistroName) = 'univention' then
      QueryUCS
    else
      QueryRepoKind;
  end
  else
  begin
    if input = 'y' then
      Data.reboot.SetEntries(rsYes, 'true')
    else
      Data.reboot.SetEntries(rsNo, 'false'); // cases input = 'n', input = ''

    QueryDhcp;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryDhcp;
begin
  writeln(rsDhcp, rsYesNoOp, '*');
  CheckInput('y,n,-b', True, rsInfoDhcp);

  if input = '-b' then
  begin
    if Data.CustomSetup then
      QueryReboot
    else
    begin
      if lowerCase(Data.DistrInfo.DistroName) = 'univention' then
        QueryUCS
      else
        QuerySetupType;
    end;
  end
  else
  begin
    if input = 'y' then
      Data.dhcp.SetEntries(rsYes, 'true')
    else
      Data.dhcp.SetEntries(rsNo, 'false'); // cases input = 'n', input = ''

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
  CheckInput('m,nom,-b', True, rsInfoTFTPROOT);

  if input = '-b' then
    QueryDhcp
  else
  begin
    if input = 'm' then
      Data.symlink := 'default.menu'
    else
      Data.symlink := 'default.nomenu'; // cases input = 'nom', input = ''

    QueryNetmask;
  end;
end;

function TQuickInstallNoQuiQuery.GetNetmaskSuggestions: string;
var
  Suggestions: string = '';
  network: array of string;
  index: integer = 0;
begin
  // IP4.ADDRESS[1]
  if NetworkDetails[index] <> '' then
  begin
    network := NetworkDetails[index].Split(['/']);
    Suggestions += getNetmaskByIP4adr(network[1]);
    // IP4.ADDRESS[2]
    Inc(index);
    if NetworkDetails[index] <> '' then
    begin
      network := NetworkDetails[index].Split(['/']);
      Suggestions += ', ' + getNetmaskByIP4adr(network[1]);
      // IP4.ADDRESS[3]
      Inc(index);
      if NetworkDetails[index] <> '' then
      begin
        network := NetworkDetails[index].Split(['/']);
        Suggestions += ', ' + getNetmaskByIP4adr(network[1]);
      end;
    end;
  end;
  Result := Suggestions;
end;

procedure TQuickInstallNoQuiQuery.QueryNetmask;
begin
  writeln(rsNetmask, rsSuggestion, GetNetmaskSuggestions, ']*');
  CheckHelp(rsInfoNetwork);

  if input = '-b' then
    QueryLink
  else
  begin
    Data.netmask := input;
    QueryNetworkAddress;
  end;
end;

function TQuickInstallNoQuiQuery.GetNetworkAddressSuggestions: string;
var
  Suggestions: string = '';
  network: array of string;
  index: integer = 0;
begin
  // IP4.ADDRESS[1]
  if NetworkDetails[index] <> '' then
  begin
    network := NetworkDetails[index].Split(['/']);
    Suggestions += getIP4NetworkByAdrAndMask(network[0], network[1]);
    // IP4.ADDRESS[2]
    Inc(index);
    if NetworkDetails[index] <> '' then
    begin
      network := NetworkDetails[index].Split(['/']);
      Suggestions += ', ' + getIP4NetworkByAdrAndMask(network[0], network[1]);
      // IP4.ADDRESS[3]
      Inc(index);
      if NetworkDetails[index] <> '' then
      begin
        network := NetworkDetails[index].Split(['/']);
        Suggestions += ', ' + getIP4NetworkByAdrAndMask(network[0], network[1]);
      end;
    end;
  end;
  Result := Suggestions;
end;

procedure TQuickInstallNoQuiQuery.QueryNetworkAddress;
begin
  writeln(rsNetworkAddress, rsSuggestion, GetNetworkAddressSuggestions, ']*');
  CheckHelp(rsInfoNetwork);

  if input = '-b' then
    QueryNetmask
  else
  begin
    Data.networkAddress := input;
    QueryDomain;
  end;
end;

function TQuickInstallNoQuiQuery.GetDomainSuggestions: string;
var
  Suggestions: string = '';
  index: integer = 3;
begin
  // IP4.DOMAIN[1]
  if NetworkDetails[index] <> '' then
  begin
    Suggestions += NetworkDetails[index];
    // IP4.DOMAIN[2]
    Inc(index);
    if NetworkDetails[index] <> '' then
    begin
      Suggestions += ', ' + NetworkDetails[index];
      // IP4.DOMAIN[3]
      Inc(index);
      if NetworkDetails[index] <> '' then
        Suggestions += ', ' + NetworkDetails[index];
    end;
  end;
  Result := Suggestions;
end;

procedure TQuickInstallNoQuiQuery.QueryDomain;
begin
  writeln(rsDomain, rsSuggestion, GetDomainSuggestions, ']*');
  CheckHelp(rsInfoNetwork);

  if input = '-b' then
    QueryNetworkAddress
  else
  begin
    Data.domain := input;
    QueryNameserver;
  end;
end;

function TQuickInstallNoQuiQuery.GetNameserverSuggestions: string;
var
  Suggestions: string = '';
  index: integer = 6;
begin
  // IP4.DNS[1]
  if NetworkDetails[index] <> '' then
  begin
    Suggestions += NetworkDetails[index];
    // IP4.DNS[2]
    Inc(index);
    if NetworkDetails[index] <> '' then
    begin
      Suggestions += ', ' + NetworkDetails[index];
      // IP4.DNS[3]
      Inc(index);
      if NetworkDetails[index] <> '' then
        Suggestions += ', ' + NetworkDetails[index];
    end;
  end;
  Result := Suggestions;
end;

procedure TQuickInstallNoQuiQuery.QueryNameserver;
begin
  writeln(rsNameserver, rsSuggestion, GetNameserverSuggestions, ']*');
  CheckHelp(rsInfoNetwork);

  if input = '-b' then
    QueryDomain
  else
  begin
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
  readln(input);
  while input = '-h' do
  begin
    writeln(rsInfoNetwork);
    readln(input);
  end;

  if input = '-b' then
    QueryNameserver
  else
  begin
    Data.gateway := input;
    QueryAdminName;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryAdminName;
begin
  writeln(rsAdminName, '*');
  CheckHelp(rsInfoAdmin);

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
  if input = '-b' then
    QueryAdminName
  else
  begin
    Data.adminPassword := input;
    QueryIPName;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryIPName;
begin
  writeln(rsIPName);
  readln(input);
  while not ((input = 'auto') or isValidFQDN(input) or (input = '-b')) do
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
    Data.ipName := input;
    QueryIPNumber;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryIPNumber;
begin
  writeln(rsIPNumber);
  readln(input);
  while not ((input = 'auto') or isValidIP4(input) or (input = '-b')) do
  begin
    writeln('"', input, '"', rsNotValid);
    readln(input);
  end;

  if input = '-b' then
    QueryIPName
  else
  begin
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

procedure AddQueryProcedureToList(QueryProcedure: TQueryProcedure;
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
    AddQueryProcedureToList(@QueryRepo, QueryProceduresList);
    AddQueryProcedureToList(@QueryProxy, QueryProceduresList);
    AddQueryProcedureToList(@QueryRepoNoCache, QueryProceduresList);
    AddQueryProcedureToList(@QueryBackend, QueryProceduresList);

    if Data.backend = 'mysql' then
      AddQueryProcedureToList(@QueryModules, QueryProceduresList);

    AddQueryProcedureToList(@QueryRepoKind, QueryProceduresList);
  end;

  {Both}
  if lowerCase(Data.DistrInfo.DistroName) = 'univention' then
    AddQueryProcedureToList(@QueryUCS, QueryProceduresList);

  {Custom installation}
  if Data.CustomSetup then
    AddQueryProcedureToList(@QueryReboot, QueryProceduresList);

  {Both}
  AddQueryProcedureToList(@QueryDhcp, QueryProceduresList);
  if Data.dhcp.PropertyEntry = 'true' then
  begin
    AddQueryProcedureToList(@QueryLink, QueryProceduresList);
    AddQueryProcedureToList(@QueryNetmask, QueryProceduresList);
    AddQueryProcedureToList(@QueryNetworkAddress, QueryProceduresList);
    AddQueryProcedureToList(@QueryDomain, QueryProceduresList);
    AddQueryProcedureToList(@QueryNameserver, QueryProceduresList);
    AddQueryProcedureToList(@QueryGateway, QueryProceduresList);
  end;

  AddQueryProcedureToList(@QueryAdminName, QueryProceduresList);
  if Data.adminName <> '' then
    AddQueryProcedureToList(@QueryAdminPassword, QueryProceduresList);

  AddQueryProcedureToList(@QueryIPName, QueryProceduresList);
  AddQueryProcedureToList(@QueryIPNumber, QueryProceduresList);
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
    JumpBackFromOverviewToQuery(QueryProceduresList[QueryIndex])
  else
    QueryFinished := True;
end;


end.
