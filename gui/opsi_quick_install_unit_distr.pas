unit opsi_quick_install_unit_distr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls,
  oslog,
  SupportedOpsiServerDistributions,
  OpsiLinuxInstaller_DistributionForm,
  opsi_quick_install_resourcestrings,
  opsiquickinstall_data,
  DistributionInfo,
  FormAppearanceFunctions;

type

  TDistribution = class(TOpsiLinuxInstallerDistributionForm)
    procedure BtnNextClick(Sender: TObject); override;
    procedure FormActivate(Sender: TObject); override;
  end;

var
  Distribution: TDistribution;

implementation

uses
  opsi_quick_install_unit_language;

{$R *.lfm}

procedure TDistribution.FormActivate(Sender: TObject);
begin
  CenterFormOnForm(self, QuickInstall);

  // position buttons here because of different layout (size of TDistribution)
  BtnBack.Left := QuickInstall.BtnBack.Left;
  BtnNext.Left := Width - BtnBack.Left - QuickInstall.BtnNextWidth;

  QuickInstall.SetInfoImageLayout(InfoDistribution);

  // show distribution suggestion
  EditDistr.Text := Data.DistrInfo.DistroName + ' ' + Data.DistrInfo.DistroRelease;

  // text by resourcestrings
  Caption := rsCapDistr;
  LabelDistr.Caption := rsDistr;
  InfoDistribution.Hint := rsInfoDistribution + #10 + SupportedDistributionsInfoString;
  LabelCorrect.Caption := rsCorrect;
  BtnBack.Caption := rsBack;
  BtnNext.Caption := rsNext;
end;

procedure TDistribution.BtnNextClick(Sender: TObject);
var
  UserEditedDistroName, UserEditedDistroRelease: string;
begin
  inherited BtnNextClick(Sender);

  // If the distribution was edited:
  if EditDistr.Text <> Data.DistrInfo.DistroName + ' ' +
    Data.DistrInfo.DistroRelease then
  begin
    // set new distribution name and release
    // function Copy is 1-based like Pos
    UserEditedDistroName := Copy(EditDistr.Text, 1, Pos(' ', EditDistr.Text) - 1);
    UserEditedDistroRelease :=
      Copy(EditDistr.Text, Pos(' ', EditDistr.Text) + 1, Length(EditDistr.Text) -
      Pos(' ', EditDistr.Text));
    Data.DistrInfo.CorrectDistribution(UserEditedDistroName,
      UserEditedDistroRelease);
  end;

  // If the distribution is not supported, show an information and close QuickInstall:
  if Data.DistrInfo.Distr = other then
  begin
    ShowMessage(rsNoSupport + #10 + #10 + SupportedDistributionsInfoString);
    FreeAndNil(LogDatei);
    FreeAndNil(Data);
    Halt(1);
  end;
end;

end.
