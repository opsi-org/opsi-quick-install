unit opsiquickinstall_InstallationScriptExecuter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  oslog,
  opsi_quick_install_resourcestrings,
  OpsiLinuxInstaller_InstallationScriptExecuter,
  opsiquickinstall_data,
  IndependentMessageDisplayer;

type

  TLOpsiServerInstallationScriptExecuter = class(TInstallationScriptExecuter)
  protected
    procedure WritePropertiesToFile; override;
    procedure ExecuteInstallationScript; override;
    function DidNewerVersionOfTwoVersionsFail: boolean; override;
    procedure TryOlderVersion; override;
    procedure LogResultOfLastInstallationAttempt; override;
  end;


implementation

// get properties from query and write them to file properties.conf
procedure TLOpsiServerInstallationScriptExecuter.WritePropertiesToFile;
begin
  FMessageDisplayer.DisplayMessage(rsDownloadLatestLOpsiServer +
    LongMessageSeperator + rsSomeMin, True);
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

  if not FileExists(FClientDataDir + 'properties.conf') then
    FInstallRunCommand.Run('touch ' + FClientDataDir + 'properties.conf', Output);
  FInstallRunCommand.Run('chown -c $USER ' + FClientDataDir +
    'properties.conf', Output);
  FFileText.SaveToFile(FClientDataDir + 'properties.conf');
end;

procedure TLOpsiServerInstallationScriptExecuter.ExecuteInstallationScript;
begin
  FMessageDisplayer.DisplayMessage(rsInstall + FCurrentVersionName +
    '... ' + LongMessageSeperator + rsSomeMin, True);

  // Important for getting the result 'failed' in case of a wrong password
  // because in this case the RunCommands below aren't executed and therefore
  // setup.opsiscript, that usually does it, isn't too:
  FFileText.Clear;
  FFileText.Add('failed');
  if not FileExists(FClientDataDir + 'result.conf') then
    FInstallRunCommand.Run('touch ' + FClientDataDir + 'result.conf', Output);

  FInstallRunCommand.Run('chown -c $USER ' + FClientDataDir + 'result.conf', Output);
  FFileText.SaveToFile(FClientDataDir + 'result.conf');

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

function TLOpsiServerInstallationScriptExecuter.DidNewerVersionOfTwoVersionsFail:
boolean;
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
  FMessageDisplayer.DisplayMessage(rsInstallation + rsFailed + '.' +
    #10 + rsTryOlderVersion + '.', True);
  LogDatei.log('Installation failed: ' + FCurrentVersionName, LLessential);
  LogDatei.log('Try older version of l-opsi-server:', LLnotice);
  FTwoVersionsToTest := False;
  FOneInstallationFailed := True;
  WritePropertiesToFile;
  ExecuteInstallationScript;
end;

procedure TLOpsiServerInstallationScriptExecuter.LogResultOfLastInstallationAttempt;
var
  ResultOfWholeInstallationProcess: string = '';
begin
  FFileText.Clear;
  FFileText.LoadFromFile(FClientDataDir + 'result.conf');
  if FFileText[0] = 'failed' then
  begin
    ResultOfWholeInstallationProcess := rsFailed;
    FMessageDisplayer.DisplayMessage(rsInstallation + rsFailed + '.', True);
    LogDatei.log('Installation failed: ' + FCurrentVersionName, LLessential);
    LogDatei.log(Data.opsiVersion + ' installation failed', LLessential);
    ExitCode := 1;
  end
  else
  begin
    ResultOfWholeInstallationProcess := rsSuccess;
    LogDatei.log('Installation successful: ' + FCurrentVersionName, LLessential);
    LogDatei.log(Data.opsiVersion + ' installation successful', LLessential);
  end;

  FMessageDisplayer.DisplayMessage(rsInstallationOf + Data.opsiVersion +
    ' ' + ResultOfWholeInstallationProcess + '!' + #10 + #10 + rsLog +
    #10 + LogOpsiServer + #10 +
    StringReplace(LogDatei.FileName, '//', '/', [rfReplaceAll]));
end;

end.
