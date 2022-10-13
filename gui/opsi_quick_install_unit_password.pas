unit opsi_quick_install_unit_password;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  MaskEdit, LCLType, cthreads, Process,
  osLog,
  FormAppearanceFunctions,
  OpsiLinuxInstaller_PasswordForm,
  opsiquickinstall_QueryData;

type

  TPassword = class(TOpsiLinuxInstallerPasswordForm)
    procedure BtnBackClick(Sender: TObject); override;
    procedure BtnFinishClick(Sender: TObject); override;
    procedure FormActivate(Sender: TObject); override;
    procedure FormClose(Sender: TObject); override;
  end;

var
  Password: TPassword;

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_overview,
  opsi_quick_install_unit_wait;

{$R *.lfm}

procedure TPassword.FormActivate(Sender: TObject);
begin
  inherited FormActivate(Sender);
  CenterFormOnForm(self, Overview);
  BtnFinish.Left := Width - BtnBack.Left - QuickInstall.BtnFinishWidth;
end;

procedure TPassword.BtnBackClick(Sender: TObject);
begin
  btnFinishClicked := False;
  Password.Visible := False;
  Overview.Enabled := True;
end;

procedure TPassword.BtnFinishClick(Sender: TObject);
begin
  if not IsPasswordCorrect then Exit;

  btnFinishClicked := True;
  Wait.Visible := True;
end;

procedure TPassword.FormClose(Sender: TObject);
begin
  if btnFinishClicked then
  begin
    if Assigned(Data) then FreeAndNil(Data);
    Overview.Close;
  end
  else
    Overview.Enabled := True;
end;

end.
