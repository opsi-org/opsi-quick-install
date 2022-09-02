unit opsiquickinstall_InstallationScriptExecuter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  oslog,
  opsi_quick_install_resourcestrings,
  OpsiLinuxInstaller_InstallationScriptExecuter,
  opsi_quick_install_unit_wait,
  opsiquickinstall_data;

type

  TLOpsiServerInstallationScriptExecuter = class(TInstallationScriptExecuter)
  protected
    procedure ShowMessageOnForm; override;
    procedure WritePropertiesToFile; override;
    procedure ExecuteInstallationScript; override;
    function DidNewerVersionOfTwoVersionsFail: boolean; override;
    procedure TryOlderVersion; override;
    procedure LogResultOfLastInstallationAttempt; override;
  end;


implementation

uses
  opsi_quick_install_unit_password;

procedure TLOpsiServerInstallationScriptExecuter.ShowMessageOnForm;
begin
  Wait.LabelWait.Caption := FMessage;
end;

// get properties from query and write them to file properties.conf
procedure TLOpsiServerInstallationScriptExecuter.WritePropertiesToFile;
begin
  FMessage := rsDownloadLatestLOpsiServer + #10 + rsSomeMin;
  //Synchronize(@ShowMessageOnForm);
  inherited WritePropertiesToFile;

  // Write user input in properties.conf file:
  FFileText.Clear;
  FFileText.Add('allow_reboot=' + Data.reboot.PropertyEntry);
  FFileText.Add('backend=' + Data.backend);
  FFileText.Add('dnsdomain=' + Data.domain);
  FFileText.Add('force_copy_modules=' + Data.copyMod.PropertyEntry);
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

  Password.clientDataDir := FClientDataDir;

  if not FileExists(FClientDataDir + 'properties.conf') then
    FInstallRunCommand.Run('touch ' + FClientDataDir + 'properties.conf', Output);
  FInstallRunCommand.Run('chown -c $USER ' + FClientDataDir +
    'properties.conf', Output);
  FFileText.SaveToFile(FClientDataDir + 'properties.conf');
end;

procedure TLOpsiServerInstallationScriptExecuter.ExecuteInstallationScript;
begin
  // Important for getting the result 'failed' in case of a wrong password
  // because in this case the RunCommands below aren't executed and therefore
  // setup.opsiscript, that usually does it, isn't too:
  FFileText.Clear;
  FFileText.Add('failed');
  if not FileExists(FClientDataDir + 'result.conf') then
    FInstallRunCommand.Run('touch ' + FClientDataDir + 'result.conf', Output);

  FInstallRunCommand.Run('chown -c $USER ' + FClientDataDir + 'result.conf', Output);
  FFileText.SaveToFile(FClientDataDir + 'result.conf');

  FInstallRunCommand.Run('./BUILD/rootfs/usr/bin/opsi-script -batch ' +
    FClientDataDir + 'setup.opsiscript /var/log/opsi-quick-install-l-opsi-server.log',
    Output);
end;

function TLOpsiServerInstallationScriptExecuter.DidNewerVersionOfTwoVersionsFail: boolean;
begin
  // get installation result from result.conf which is filled by the l-opsi-server
  FFileText.Clear;
  FFileText.LoadFromFile(FClientDataDir + 'result.conf');

  if (FFileText[0] = 'failed') and FTwoVersionsToTest then
    Result := True
  else
    Result := False;
end;

procedure TLOpsiServerInstallationScriptExecuter.TryOlderVersion;
begin
  //FMessage := rsInstallation + rsFailed + '.' + #10 + rsTryOlderVersion + '.';
  //Synchronize(@ShowMessageOnForm);
  Sleep(1000);
  LogDatei.log('Installation failed: ' + FCurrentVersionName, LLessential);
  LogDatei.log('Try older version of l-opsi-server:', LLnotice);
  FTwoVersionsToTest := False;
  FOneInstallationFailed := True;
  WritePropertiesToFile;

  ExecuteInstallationScript;
  FFileText.LoadFromFile(FClientDataDir + 'result.conf');
end;

procedure TLOpsiServerInstallationScriptExecuter.LogResultOfLastInstallationAttempt;
begin
  if FFileText[0] = 'failed' then
    LogDatei.log('Installation failed: ' + FCurrentVersionName, LLessential)
  else
    LogDatei.log('Installation successful: ' + FCurrentVersionName, LLessential);
end;

end.
