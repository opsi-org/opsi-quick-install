unit opsi_quick_install_unit_wait;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  opsiquickinstall_InstallationScriptExecuter,
  IndependentMessageDisplayer,
  opsiquickinstall_QueryData,
  FormAppearanceFunctions;

type
  TWait = class(TForm)
    LabelWait: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  end;

var
  Wait: TWait;

implementation

uses
  opsi_quick_install_unit_password;

{$R *.lfm}

procedure TWait.FormActivate(Sender: TObject);
var
  LOpsiServerInstallationScriptExecuter: TLOpsiServerInstallationScriptExecuter;
  MessageDisplayer: TIndependentMessageDisplayer;
begin
  CenterFormOnForm(self, Password);

  MessageDisplayer := TIndependentMessageDisplayer.Create(LabelWait);
  LOpsiServerInstallationScriptExecuter :=
    TLOpsiServerInstallationScriptExecuter.Create(Password.EditPassword.Text,
    Password.RadioBtnSudo.Checked, Data.DistrInfo.PackageManagementShellCommand,
    'l-opsi-server', 'download.uib.de/opsi4.2/testing/packages/linux/localboot/',
    MessageDisplayer);

  Application.ProcessMessages;
  LOpsiServerInstallationScriptExecuter.InstallOpsiProduct;

  // Closing TWait recursively closes all forms
  Close;
end;

procedure TWait.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  Password.Close;
end;

end.
