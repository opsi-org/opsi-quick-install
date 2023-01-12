unit opsiquickinstall_InstallationScriptExecuter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  oslog,
  OpsiLinuxInstaller_InstallationScriptExecuter,
  opsiquickinstall_QueryData,
  opsi_quick_install_CommonResourceStrings;

type

  TLOpsiServerInstallationScriptExecuter = class(TInstallationScriptExecuter)
  protected
    procedure WritePropertiesToFile; override;
    procedure ExecuteInstallationScript; override;
    function DidNewerVersionOfTwoVersionsFail: boolean; override;
    procedure LogResultOfLastInstallationAttempt; override;
  end;


implementation

// get properties from query and write them to file properties.conf
procedure TLOpsiServerInstallationScriptExecuter.WritePropertiesToFile;
begin
  inherited WritePropertiesToFile;

  // Write user input in properties.conf file:
  FFileText.Clear;
  FFileText.Add('allow_reboot=' + Data.reboot.PropertyEntry);
  FFileText.Add('backend=' + Data.backend);
  FFileText.Add('dnsdomain=' + Data.domain);
  FFileText.Add('force_copy_modules=' + Data.copyMod.PropertyEntry);
  FFileText.Add('gateway=' + Data.gateway);
  FFileText.Add('grafana_online_repository=' + Data.grafanaRepo);
  FFileText.Add('install_and_configure_dhcp=' + Data.dhcp.PropertyEntry);
  FFileText.Add('myipname=' + Data.ipName);
  FFileText.Add('myipnumber=' + Data.ipNumber);
  FFileText.Add('nameserver=' + Data.nameserver);
  FFileText.Add('netmask=' + Data.netmask);
  FFileText.Add('network=' + Data.networkAddress);
  FFileText.Add('opsi_admin_user_name=' + Data.adminName);
  FFileText.Add('opsi_admin_user_password=' + Data.adminPassword);
  FFileText.Add('opsi_online_repository=' + Data.repo);
  FFileText.Add('opsi_noproxy_online_repository=' + Data.repoNoCache);
  FFileText.Add('patch_default_link_for_bootimage=' + Data.symlink);
  FFileText.Add('proxy=' + Data.proxy);
  FFileText.Add('repo_kind=' + Data.repoKind);
  FFileText.Add('ucs_master_admin_password=' + Data.ucsPassword);
  // update_test shall always be false
  FFileText.Add('update_test=false');

  if not FileExists(FClientDataDir + 'properties.conf') then
    FInstallRunCommand.Run('touch ' + FClientDataDir + 'properties.conf', Output);
  FInstallRunCommand.Run('chown -c $USER ' + FClientDataDir +
    'properties.conf', Output);
  FFileText.SaveToFile(FClientDataDir + 'properties.conf');
end;

procedure TLOpsiServerInstallationScriptExecuter.ExecuteInstallationScript;
var
  ResultFileText: TStringList;
begin
  inherited ExecuteInstallationScript;

  // Important for getting the result 'failed' in case of a wrong password
  // because in this case the RunCommands below aren't executed and therefore
  // setup.opsiscript, that usually does it, isn't too:
  ResultFileText := TStringList.Create;
  ResultFileText.Add('failed');
  if not FileExists(FClientDataDir + 'result.conf') then
    FInstallRunCommand.Run('touch ' + FClientDataDir + 'result.conf', Output);

  FInstallRunCommand.Run('chown -c $USER ' + FClientDataDir + 'result.conf', Output);
  ResultFileText.SaveToFile(FClientDataDir + 'result.conf');
  FreeAndNil(ResultFileText);

  {$IFDEF GUI}
  FInstallRunCommand.Run('./BUILD/rootfs/usr/bin/opsi-script -batch ' +
    FClientDataDir + 'setup.opsiscript /var/log/opsi-quick-install-l-opsi-server.log',
    Output);
  {$ENDIF GUI}

  {$IFDEF NOGUI}
  FInstallRunCommand.Run('./BUILD/rootfs/usr/bin/opsi-script -silent -batch ' +
    FClientDataDir + 'setup.opsiscript /var/log/opsi-quick-install-l-opsi-server.log',
    Output);
  {$ENDIF NOGUI}
end;

function TLOpsiServerInstallationScriptExecuter.DidNewerVersionOfTwoVersionsFail: boolean;
var
  ResultFileText: TStringList;
begin
  // get installation result from result.conf which is filled by the l-opsi-server
  ResultFileText := TStringList.Create;
  ResultFileText.LoadFromFile(FClientDataDir + 'result.conf');
  if (ResultFileText[0] = 'failed') and FTwoVersionsToTest then
    Result := True
  else
    Result := False;

  FreeAndNil(ResultFileText);
end;

procedure TLOpsiServerInstallationScriptExecuter.LogResultOfLastInstallationAttempt;
var
  ResultFileText: TStringList;
begin
  ResultFileText := TStringList.Create;
  ResultFileText.LoadFromFile(FClientDataDir + 'result.conf');
  LogResultOfLastInstallationAttempt(ResultFileText[0], Data.opsiVersion, LogOpsiServer);
  FreeAndNil(ResultFileText);
end;

end.
