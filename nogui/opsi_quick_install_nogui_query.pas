unit opsi_quick_install_nogui_query;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  opsiquickinstall_data,
  opsi_quick_install_resourcestrings,
  DistributionInfo,
  osLinuxRepository,
  osnetworkcalculator,
  osnetutil;

type
  TQuickInstallNoQuiQuery = class(TObject)
  private
    input: string;
    NetworkDetails: array of string;

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
    procedure QueryNetmask;
    procedure QueryNetworkAddress;
    procedure QueryDomain;
    procedure QueryNameserver;
    procedure QueryGateway;
    procedure QueryAdminName;
    procedure QueryAdminPassword;
    procedure QueryIPName;
    procedure QueryIPNumber;
    procedure JumpBackFromOverviewToQuery(QueryNumber: integer);
    procedure QueryOverview;
  public
    QueryFinished: boolean;
    procedure QueryDistribution;
  end;



implementation

// Input variables not set by resourcestrings but by characters for no
// requirement of a mouse.

procedure TQuickInstallNoQuiQuery.QueryDistribution;
var
  UserEditedDistroName, UserEditedDistroRelease: string;
begin
  writeln(rsDistr, ' ', Data.DistrInfo.DistroName, ' ', Data.DistrInfo.DistroRelease);
  writeln(rsIsCorrect, rsYesNoOp, '*');
  readln(input);
  while not ((input = 'y') or (input = 'n') or (input = '')) do
  begin
    if input = '-h' then
      writeln(rsInfoDistribution + #10 + Data.DistrInfo.Distribs)
    else
      writeln('"', input, '"', rsNotValid);
    readln(input);
  end;
  // if distribution isn't correct, read the correct one
  if input = 'n' then
  begin
    writeln(rsOtherDistr);
    readln(input);
    UserEditedDistroName := Copy(input, 1, Pos(' ', input) - 1);
    UserEditedDistroRelease :=
      Copy(input, Pos(' ', input) + 1, Length(input) - Pos(' ', input));
    Data.DistrInfo.CorrectDistributionNameAndRelease(UserEditedDistroName,
      UserEditedDistroRelease);
  end;
  Data.DistrInfo.SetDistrAndUrlPart;
  if Data.DistrInfo.Distr = other then
  begin
    writeln('');
    writeln(rsNoSupport + #10 + Data.DistrInfo.Distribs);
    Exit;
  end;
  QuerySetupType;
end;

procedure TQuickInstallNoQuiQuery.QuerySetupType;
begin
  writeln(rsSetup, rsSetupOp);
  readln(input);
  while not ((input = 's') or (input = 'c') or (input = '-b') or (input = '')) do
  begin
    writeln('"', input, '"', rsNotValid);
    readln(input);
  end;
  // if input = -b then go back to the previous query
  if input = '-b' then
    QueryDistribution
  else
  begin
    if input = 'c' then
      Data.CustomSetup := True
    else // cases input = 's', input = ''
      Data.CustomSetup := False;

    writeln('');
    writeln(rsCarryOut);
    writeln('');
    if Data.CustomSetup then
      // following queries only for custom setup
      //QueryOpsiVersion
      QueryRepo
    else
    if Data.DistrInfo.DistroName = 'Univention' then
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
  readln(input);
  while not ((input = 'y') or (input = 'n') or (input = '-b') or (input = '')) do
  begin
    writeln('"', input, '"', rsNotValid);
    readln(input);
  end;

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
  readln(input);
  while not ((input = 'f') or (input = 'm') or (input = '-b') or (input = '')) do
  begin
    if input = '-h' then
      writeln(rsInfoBackend)
    else
      writeln('"', input, '"', rsNotValid);
    readln(input);
  end;

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
  readln(input);
  while not ((input = 'y') or (input = 'n') or (input = '-b') or (input = '')) do
  begin
    if input = '-h' then
      writeln(rsInfoModules)
    else
      writeln('"', input, '"', rsNotValid);
    readln(input);
  end;

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
  readln(input);
  while not ((input = 'e') or (input = 't') or (input = 's') or
      (input = '-b') or (input = '')) do
  begin
    if input = '-h' then
      writeln(rsInfoRepoKind)
    else
      writeln('"', input, '"', rsNotValid);
    readln(input);
  end;

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

    if Data.DistrInfo.DistroName = 'Univention' then
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
  readln(input);
  while not ((input = 'y') or (input = 'n') or (input = '-b') or (input = '')) do
  begin
    if input = '-h' then
      writeln(rsInfoReboot)
    else
      writeln('"', input, '"', rsNotValid);
    readln(input);
  end;

  if input = '-b' then
  begin
    if Data.DistrInfo.DistroName = 'Univention' then
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
  readln(input);
  while not ((input = 'y') or (input = 'n') or (input = '-b') or (input = '')) do
  begin
    if input = '-h' then
      writeln(rsInfoDhcp)
    else
      writeln('"', input, '"', rsNotValid);
    readln(input);
  end;

  if input = '-b' then
  begin
    if Data.CustomSetup then
      QueryReboot
    else
    begin
      if Data.DistrInfo.DistroName = 'Univention' then
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
      // read in network details for dhcp queries (requires unit "osnetworkcalculator")
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
  while not ((input = 'm') or (input = 'nom') or (input = '-b') or (input = '')) do
  begin
    if input = '-h' then
      writeln(rsInfoTFTPROOT)
    else
      writeln('"', input, '"', rsNotValid);
    readln(input);
  end;
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

procedure TQuickInstallNoQuiQuery.QueryNetmask;
var
  suggestion: string;
  network: array of string;
  index: integer;
begin
  // get netmask suggestions
  suggestion := '';
  // IP4.ADDRESS[1]
  index := 0;
  if NetworkDetails[index] <> '' then
  begin
    network := NetworkDetails[index].Split(['/']);
    suggestion += getNetmaskByIP4adr(network[1]) + ', ';
    // IP4.ADDRESS[2]
    index += 1;
    if NetworkDetails[index] <> '' then
    begin
      network := NetworkDetails[index].Split(['/']);
      suggestion += getNetmaskByIP4adr(network[1]) + ', ';
      // IP4.ADDRESS[3]
      index += 1;
      if NetworkDetails[index] <> '' then
      begin
        network := NetworkDetails[index].Split(['/']);
        suggestion += getNetmaskByIP4adr(network[1]) + ', ';
      end;
    end;
    Delete(suggestion, suggestion.Length - 1, 2);
    suggestion += ']';

    // query:
    writeln(rsNetmask, rsSuggestion, suggestion, '*');
    readln(input);
    while input = '-h' do
    begin
      writeln(rsInfoNetwork);
      readln(input);
    end;

    if input = '-b' then
      QueryLink
    else
    begin
      Data.netmask := input;
      QueryNetworkAddress;
    end;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryNetworkAddress;
var
  suggestion: string;
  network: array of string;
  index: integer;
begin
  suggestion := '';
  // IP4.ADDRESS[1]
  index := 0;
  if NetworkDetails[index] <> '' then
  begin
    network := NetworkDetails[index].Split(['/']);
    suggestion += getIP4NetworkByAdrAndMask(network[0], network[1]) + ', ';
    // IP4.ADDRESS[2]
    index += 1;
    if NetworkDetails[index] <> '' then
    begin
      network := NetworkDetails[index].Split(['/']);
      suggestion += getIP4NetworkByAdrAndMask(network[0], network[1]) + ', ';
      // IP4.ADDRESS[3]
      index += 1;
      if NetworkDetails[index] <> '' then
      begin
        network := NetworkDetails[index].Split(['/']);
        suggestion += getIP4NetworkByAdrAndMask(network[0], network[1]) + ', ';
      end;
    end;
  end;
  Delete(suggestion, suggestion.Length - 1, 2);
  suggestion += ']';

  // query:
  writeln(rsNetworkAddress, rsSuggestion, suggestion, '*');
  readln(input);
  while input = '-h' do
  begin
    writeln(rsInfoNetwork);
    readln(input);
  end;

  if input = '-b' then
    QueryNetmask
  else
  begin
    Data.networkAddress := input;
    QueryDomain;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryDomain;
var
  suggestion: string;
  index: integer;
begin
  suggestion := '';
  // IP4.DOMAIN[1]
  index := 3;
  if NetworkDetails[index] <> '' then
  begin
    suggestion += NetworkDetails[index] + ', ';
    // IP4.DOMAIN[2]
    index += 1;
    if NetworkDetails[index] <> '' then
    begin
      suggestion += NetworkDetails[index] + ', ';
      // IP4.DOMAIN[3]
      index += 1;
      if NetworkDetails[index] <> '' then
        suggestion += NetworkDetails[index] + ', ';
    end;
  end;
  Delete(suggestion, suggestion.Length - 1, 2);
  suggestion += ']';

  // query:
  writeln(rsDomain, rsSuggestion, suggestion, '*');
  readln(input);
  while input = '-h' do
  begin
    writeln(rsInfoNetwork);
    readln(input);
  end;

  if input = '-b' then
    QueryNetworkAddress
  else
  begin
    Data.domain := input;
    QueryNameserver;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryNameserver;
var
  suggestion: string;
  index: integer;
begin
  suggestion := '';
  // IP4.DNS[1]
  index := 6;
  if NetworkDetails[index] <> '' then
  begin
    suggestion += NetworkDetails[index] + ', ';
    // IP4.DNS[2]
    index += 1;
    if NetworkDetails[index] <> '' then
    begin
      suggestion += NetworkDetails[index] + ', ';
      // IP4.DNS[3]
      index += 1;
      if NetworkDetails[index] <> '' then
        suggestion += NetworkDetails[index] + ', ';
    end;
    Delete(suggestion, suggestion.Length - 1, 2);
    suggestion += ']';

    // query:
    writeln(rsNameserver, rsSuggestion, suggestion, '*');
    readln(input);
    while input = '-h' do
    begin
      writeln(rsInfoNetwork);
      readln(input);
    end;

    if input = '-b' then
      QueryDomain
    else
    begin
      Data.nameserver := input;
      QueryGateway;
    end;
  end;
end;

procedure TQuickInstallNoQuiQuery.QueryGateway;
var
  suggestion: string;
begin
  suggestion := '';
  // IP4.GATEWAY
  if NetworkDetails[9] <> '' then
    suggestion += NetworkDetails[9];
  suggestion += ']';

  // query:
  writeln(rsGateway, rsSuggestion, suggestion, '*');
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
  readln(input);
  while input = '-h' do
  begin
    writeln(rsInfoAdmin);
    readln(input);
  end;

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


procedure TQuickInstallNoQuiQuery.JumpBackFromOverviewToQuery(QueryNumber: integer);
begin
  if QueryNumber = 2 then
    QueryRepo
  else
  if QueryNumber = 3 then
    QueryProxy
  else
  if QueryNumber = 4 then
    QueryRepoNoCache
  else
  if QueryNumber = 5 then
    QueryBackend
  else
  if QueryNumber = 6 then
    QueryModules
  else
  if QueryNumber = 7 then
    QueryRepoKind
  else
  if QueryNumber = 8 then
    QueryUCS
  else
  if QueryNumber = 9 then
    QueryReboot
  else
  if QueryNumber = 10 then
    QueryDhcp
  else
  if QueryNumber = 11 then
    QueryLink
  else
  if QueryNumber = 12 then
    QueryNetmask
  else
  if QueryNumber = 13 then
    QueryNetworkAddress
  else
  if QueryNumber = 14 then
    QueryDomain
  else
  if QueryNumber = 15 then
    QueryNameserver
  else
  if QueryNumber = 16 then
    QueryGateway
  else
  if QueryNumber = 17 then
    QueryAdminName
  else
  if QueryNumber = 18 then
    QueryAdminPassword
  else
  if QueryNumber = 19 then
    QueryIPName
  else
  if QueryNumber = 20 then
    QueryIPNumber;
end;


procedure TQuickInstallNoQuiQuery.QueryOverview;
var
  // number of asked questions
  Counter: integer;
  // list of the asked questions by numbers
  queries: TStringList;
  QueryIndex: integer;
  ValidQueryIndex: boolean = False;
  QueryNumber: integer;
begin
  Counter := 1;
  queries := TStringList.Create;

  // Overview
  // Print the overview and in 'queries' save the questions that were asked
  // (depending on setup type and distribution=Univention) by their number:
  writeln('');
  writeln(rsOverview);
  if not Data.CustomSetup then
    writeln(rsOpsiVersionO, Data.opsiVersion)
  else
  begin
    writeln(Counter, ' ', rsOpsiVersionO, Data.opsiVersion);
    queries.Add('1');
    Inc(Counter);
  end;
  {Custom installation}
  if Data.CustomSetup then
  begin
    writeln(Counter, ' ', rsRepoO, Data.repo);
    queries.Add('2');
    Inc(Counter);
    writeln(Counter, ' ', rsProxyO, Data.proxy);
    queries.Add('3');
    Inc(Counter);
    writeln(Counter, ' ', rsRepoNoCacheO, Data.repoNoCache);
    queries.Add('4');
    Inc(Counter);
    writeln(Counter, ' ', rsBackendO, Data.backend);
    queries.Add('5');
    Inc(Counter);
    if Data.backend = 'mysql' then
    begin
      writeln(Counter, ' ', rsCopyModulesO, Data.copyMod.OverviewEntry);
      queries.Add('6');
      Inc(Counter);
    end;
    writeln(Counter, ' ', rsRepoKindO, Data.repoKind);
    queries.Add('7');
    Inc(Counter);
  end;
  {Both}
  if Data.DistrInfo.DistroName = 'Univention' then
  begin
    writeln(Counter, ' ', rsUCSO, Data.ucsPassword);
    queries.Add('8');
    Inc(Counter);
  end;
  {Custom installation}
  if Data.CustomSetup then
  begin
    writeln(Counter, ' ', rsRebootO, Data.reboot.OverviewEntry);
    queries.Add('9');
    Inc(Counter);
  end;
  {Both}
  writeln(Counter, ' ', rsDhcpO, Data.dhcp.OverviewEntry);
  queries.Add('10');
  Inc(Counter);
  if Data.dhcp.PropertyEntry = 'true' then
  begin
    writeln(Counter, ' ', rsTFTPROOTO, Data.symlink);
    queries.Add('11');
    Inc(Counter);
    writeln(Counter, ' ', rsNetmaskO, Data.netmask);
    queries.Add('12');
    Inc(Counter);
    writeln(Counter, ' ', rsNetworkO, Data.networkAddress);
    queries.Add('13');
    Inc(Counter);
    writeln(Counter, ' ', rsDomainO, Data.domain);
    queries.Add('14');
    Inc(Counter);
    writeln(Counter, ' ', rsNameserverO, Data.nameserver);
    queries.Add('15');
    Inc(Counter);
    writeln(Counter, ' ', rsGatewayO, Data.gateway);
    queries.Add('16');
    Inc(Counter);
  end;
  writeln(Counter, ' ', rsAdminNameO, Data.adminName);
  queries.Add('17');
  Inc(Counter);
  if Data.adminName <> '' then
  begin
    writeln(Counter, ' ', rsAdminPasswordO, Data.adminPassword);
    queries.Add('18');
    Inc(Counter);
  end;
  writeln(Counter, ' ', rsIPNameO, Data.ipName);
  queries.Add('19');
  Inc(Counter);
  writeln(Counter, ' ', rsIPNumberO, Data.ipNumber);
  queries.Add('20');
  //writeln(queries.Text);

  writeln('');
  writeln(rsContinue);
  // Jumping back to a query by the number in the overview:
  readln(input);
  try
    QueryIndex := StrToInt(input) - 1;
    QueryNumber := StrToInt(queries[QueryIndex]);
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
      QueryNumber := StrToInt(queries[QueryIndex]);
      ValidQueryIndex := True;
    except
    end;
  end;
  // jump back to the respective question
  if input <> '' then
    JumpBackFromOverviewToQuery(QueryNumber)
  else
    QueryFinished := True;
end;


end.
