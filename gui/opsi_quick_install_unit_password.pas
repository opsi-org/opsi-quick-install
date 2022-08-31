unit opsi_quick_install_unit_password;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  MaskEdit, LCLType, cthreads, Process,
  osRunCommandElevated,
  osLog,
  FormAppearanceFunctions,
  OpsiLinuxInstaller_PasswordForm;

type

  TPassword = class(TOpsiLinuxInstallerPasswordForm)
    procedure BtnBackClick(Sender: TObject); override;
    procedure BtnFinishClick(Sender: TObject); override;
    procedure FormActivate(Sender: TObject); override;
    procedure ShowResultOfWholeInstallationProcess; override;
    procedure CloseProject; override;
    procedure FormClose(Sender: TObject); override;
  end;

  // Thread for showing 'please wait' on a form while 'apt/zypper/.. update'
  // runs in the background.
  TInstallOpsiThread = class(TOpsiLinuxInstallerThread)
  protected
    procedure ShowMessageOnForm; override;
    procedure WritePropertiesToFile; override;
    procedure GetOpsiScript; override;
    procedure ExecuteInstallationScript; override;
    function DidNewerVersionOfTwoVersionsFail: boolean; override;
    procedure TryOlderVersion; override;
    procedure LogResultOfLastInstallationAttempt; override;

  end;

var
  Password: TPassword;
  InstallOpsiThread: TInstallOpsiThread;

implementation

uses
  opsi_quick_install_resourcestrings,
  opsiquickinstall_data,
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_overview,
  opsi_quick_install_unit_wait,
  DistributionInfo;

{$R *.lfm}

{InstallOpsiThread}

procedure TInstallOpsiThread.ShowMessageOnForm;
begin
  Wait.LabelWait.Caption := FMessage;
end;

// get properties from query and write them to file properties.conf
procedure TInstallOpsiThread.WritePropertiesToFile;
begin
  FMessage := rsDownloadLatestLOpsiServer + #10 + rsSomeMin;
  Synchronize(@ShowMessageOnForm);
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

procedure TInstallOpsiThread.GetOpsiScript;
begin
  // Get opsi-script_*.tar.gz from download.opensuse.org and extract it to the directory of the binary
  FPackageManagementShellCommand :=
    GetPackageManagementShellCommand(Data.DistrInfo.DistroName);
  inherited GetOpsiScript;
end;

procedure TInstallOpsiThread.ExecuteInstallationScript;
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

  FInstallRunCommand.Run('./BUILD/rootfs/usr/bin/opsi-script -silent -batch ' +
    FClientDataDir + 'setup.opsiscript /var/log/opsi-quick-install-l-opsi-server.log',
    Output);
end;

function TInstallOpsiThread.DidNewerVersionOfTwoVersionsFail: boolean;
begin
  // get installation result from result.conf which is filled by the l-opsi-server
  FFileText.Clear;
  FFileText.LoadFromFile(FClientDataDir + 'result.conf');

  if (FFileText[0] = 'failed') and FTwoVersionsToTest then
    Result := True
  else
    Result := False;
end;

procedure TInstallOpsiThread.TryOlderVersion;
begin
  FMessage := rsInstallation + rsFailed + '.' + #10 + rsTryOlderVersion + '.';
  Synchronize(@ShowMessageOnForm);
  Sleep(1000);
  LogDatei.log('Installation failed: ' + FCurrentVersionName, LLessential);
  LogDatei.log('Try older version of l-opsi-server:', LLnotice);
  FTwoVersionsToTest := False;
  FOneInstallationFailed := True;
  WritePropertiesToFile;

  ExecuteInstallationScript;
  FFileText.LoadFromFile(FClientDataDir + 'result.conf');
end;


procedure TInstallOpsiThread.LogResultOfLastInstallationAttempt;
begin
  if FFileText[0] = 'failed' then
    LogDatei.log('Installation failed: ' + FCurrentVersionName, LLessential)
  else
    LogDatei.log('Installation successful: ' + FCurrentVersionName, LLessential);
end;


{ TPassword }

procedure TPassword.ShowResultOfWholeInstallationProcess;
var
  FileText: TStringList;
  InstallationResult: string;
begin
  FileText := TStringList.Create;
  FileText.LoadFromFile(clientDataDir + 'result.conf');

  // adjust quick-install ExitCode
  if FileText[0] = 'failed' then
  begin
    InstallationResult := rsFailed;
    LogDatei.log(Data.opsiVersion + ' installation failed', LLessential);
    ExitCode := 1;
  end
  else
  begin
    InstallationResult := rsSuccess;
    LogDatei.log(Data.opsiVersion + ' installation successful', LLessential);
  end;

  ShowMessage(rsInstallationOf + Data.opsiVersion + ' ' + installationResult +
    '!' + #10 + #10 + rsLog + #10 + LogOpsiServer + #10 +
    LogDatei.StandardMainLogPath + QuickInstall.LogFileName);
  FreeAndNil(FileText);
end;

procedure TPassword.FormActivate(Sender: TObject);
begin
  inherited FormActivate(Sender);
  CenterFormOnForm(self, Overview);
  BtnFinish.Left := Width - BtnBack.Left - QuickInstall.BtnFinishWidth;

  // text by resourcestrings
  Caption := rsPassword;
  LabelRights.Caption := rsRights;
  LabelPassword.Caption := rsPassword + ':';
  CheckBoxShowPassword.Caption := rsShowPassword;
  BtnBack.Caption := rsBack;
  BtnFinish.Caption := rsFinish;
end;

procedure TPassword.BtnBackClick(Sender: TObject);
begin
  btnFinishClicked := False;
  Password.Visible := False;
  Overview.Enabled := True;
end;

procedure TPassword.BtnFinishClick(Sender: TObject);
begin
  if not IsPasswordCorrect(rsWrongPassword) then Exit;

  btnFinishClicked := True;

  // start thread for opsi server installation while showing TWait
  InstallOpsiThread := TInstallOpsiThread.Create(EditPassword.Text,
    RadioBtnSudo.Checked, Data.DistrInfo.PackageManagementShellCommand,
    'l-opsi-server', 'download.uib.de/opsi4.2/testing/packages/linux/localboot/');
  InstallOpsiThread.OnTerminate := @FormClose;
  InstallOpsiThread.Start;
  Wait.Visible := True;
end;

procedure TPassword.CloseProject;
begin
  Overview.Close;
  Wait.Close;
end;

procedure TPassword.FormClose(Sender: TObject);
begin
  if btnFinishClicked then
  begin
    ShowResultOfWholeInstallationProcess;
    Data.DistrInfo.Free;
    FreeAndNil(Data);
    CloseProject;
  end
  else
    Overview.Enabled := True;
end;

end.
