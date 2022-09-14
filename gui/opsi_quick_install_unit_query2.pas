unit opsi_quick_install_unit_query2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  OpsiLinuxInstaller_QueryForm,
  FormAppearanceFunctions,
  opsi_quick_install_resourcestrings,
  opsiquickinstall_QueryData;

type

  TQuery2 = class(TOpsiLinuxInstallerQueryForm)
    InfoBackend: TImage;
    InfoModules: TImage;
    InfoRepoKind: TImage;
    LabelCopyModules: TLabel;
    LabelRepoKind: TLabel;
    LabelBackend: TLabel;
    PanelCopyModules: TPanel;
    PanelRadio: TPanel;
    PanelRadioRepoKind: TPanel;
    PanelRadioBackend: TPanel;
    PanelRepoKind: TPanel;
    PanelBackend: TPanel;
    RadioBtnFile: TRadioButton;
    RadioBtnMySql: TRadioButton;
    RadioBtnExperimental: TRadioButton;
    RadioBtnNoCopy: TRadioButton;
    RadioBtnStable: TRadioButton;
    RadioBtnTesting: TRadioButton;
    RadioBtnYesCopy: TRadioButton;
    procedure BtnBackClick(Sender: TObject); override;
    procedure BtnNextClick(Sender: TObject); override;
    procedure FormActivate(Sender: TObject); override;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction); override;
    procedure RadioBtnFileChange(Sender: TObject);
  end;

var
  Query2: TQuery2;

implementation

uses
  opsi_quick_install_unit_query,
  opsi_quick_install_unit_query4;

{$R *.lfm}

procedure TQuery2.BtnNextClick(Sender: TObject);
begin
  // Make Data entries:
  // Backend
  if RadioBtnFile.Checked then
    Data.backend := RadioBtnFile.Caption
  else
    Data.backend := RadioBtnMySql.Caption;
  // Copy modules
  if RadioBtnYesCopy.Checked then
    Data.copyMod.SetEntries(RadioBtnYesCopy.Caption, 'true')
  else
    Data.copyMod.SetEntries(RadioBtnNoCopy.Caption, 'false');
  // Repo kind
  if RadioBtnStable.Checked then
    Data.repoKind := RadioBtnStable.Caption
  else if RadioBtnTesting.Checked then
    Data.repoKind := RadioBtnTesting.Caption
  else
    Data.repoKind := RadioBtnExperimental.Caption;

  // Query3 doesn't exist any more
  showForm(Query4, self);
  Query4.BtnBack.Left := BtnBack.Left;
  Query4.BtnBack.Top := BtnBack.Top;
  Query4.BtnNext.Left := BtnNext.Left;
  Query4.BtnNext.Top := BtnNext.Top;
end;

procedure TQuery2.FormActivate(Sender: TObject);
begin
  inherited FormActivate(Sender);

  PanelCopyModules.AutoSize := False;
  PanelCopyModules.AutoSize := True;
  // so far opsi 4.2 only has the branches experimental and testing
  if RadioBtnStable.Visible = False then
    RadioBtnTesting.BorderSpacing.Left := 0;

  // text by resourcestrings
  Caption := 'Opsi Quick Install - ' + rsCapQuery2;
  LabelBackend.Caption := rsBackend;
  InfoBackend.Hint := rsInfoBackend;
  LabelCopyModules.Caption := rsCopyModules;
  InfoModules.Hint := rsInfoModules;
  RadioBtnYesCopy.Caption := rsYes;
  RadioBtnNoCopy.Caption := rsNo;
  LabelRepoKind.Caption := rsRepoKind;
  InfoRepoKind.Hint := rsInfoRepoKind;
  BtnBack.Caption := rsBack;
  BtnNext.Caption := rsNext;
end;

procedure TQuery2.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  Query.Close;
end;

procedure TQuery2.RadioBtnFileChange(Sender: TObject);
begin
  // ask for copying modules only if backend is mysql
  if RadioBtnFile.Checked then
  begin
    PanelCopyModules.Visible := False;
    InfoModules.Visible := False;
  end
  else
  begin
    PanelCopyModules.Visible := True;
    InfoModules.Visible := True;
  end;
end;

procedure TQuery2.BtnBackClick(Sender: TObject);
begin
  showForm(Query, self);
end;

end.
