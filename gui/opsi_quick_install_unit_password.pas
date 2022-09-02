unit opsi_quick_install_unit_password;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  MaskEdit, LCLType, cthreads, Process,
  osLog,
  FormAppearanceFunctions,
  OpsiLinuxInstaller_PasswordForm,
  opsiquickinstall_InstallationScriptExecuter;

type

  TPassword = class(TOpsiLinuxInstallerPasswordForm)
    procedure BtnBackClick(Sender: TObject); override;
    procedure BtnFinishClick(Sender: TObject); override;
    procedure FormActivate(Sender: TObject); override;
    procedure ShowResultOfWholeInstallationProcess; override;
    procedure CloseProject; override;
    procedure FormClose(Sender: TObject); override;
  end;

  // Thread for showing infos on a form while installation runs in the background.
  TInstallOpsiThread = class(TThread)
    LOpsiServerInstallationScriptExecuter: TLOpsiServerInstallationScriptExecuter;
  public
    constructor Create(password: string; sudo: boolean;
      PackageManagementShellCommand: string; ProductID: string; DownloadPath: string);
    procedure Execute; override;
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

constructor TInstallOpsiThread.Create(password: string; sudo: boolean;
  PackageManagementShellCommand: string; ProductID: string; DownloadPath: string);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  LOpsiServerInstallationScriptExecuter := TLOpsiServerInstallationScriptExecuter.Create(password, sudo,
    PackageManagementShellCommand, ProductID, DownloadPath);
end;

procedure TInstallOpsiThread.Execute;
begin
  // sleep to ensure that TWait is shown before GetOpsiScript is executed and blocks TWait
  Sleep(100);
  LOpsiServerInstallationScriptExecuter.InstallOpsiProduct;
  //FFileText.Free;
  //FInstallRunCommand.Free;
  FreeAndNil(LOpsiServerInstallationScriptExecuter);
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
