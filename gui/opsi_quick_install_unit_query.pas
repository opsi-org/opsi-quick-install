unit opsi_quick_install_unit_query;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls,
  OpsiLinuxInstaller_QueryForm,
  FormAppearanceFunctions,
  opsiquickinstall_QueryData,
  opsi_quick_install_CommonResourceStrings,
  opsi_quick_install_GuiResourceStrings,
  oslog;

type

  TQuery = class(TOpsiLinuxInstallerQueryForm)
    EditDefaultRepoNoCache: TEdit;
    EditDefaultRepo: TEdit;
    EditProxy: TEdit;
    EditRepo: TEdit;
    EditOtherNoCache: TEdit;
    InfoRepo: TImage;
    InfoOpsiVersion: TImage;
    LabelOpsiVersion: TLabel;
    LabelNoCache: TLabel;
    LabelProxy: TLabel;
    LabelRepo: TLabel;
    PanelOpsiVersion: TPanel;
    PanelNoCache: TPanel;
    PanelProxy: TPanel;
    PanelRepo: TPanel;
    RadioBtnRepo: TRadioButton;
    RadioBtnNone: TRadioButton;
    RadioBtnOtherRepo: TRadioButton;
    RadioBtnOtherProxy: TRadioButton;
    RadioBtnOtherNoCache: TRadioButton;
    RadioBtnRepoNoCache: TRadioButton;
    RadioBtnMyProxy: TRadioButton;
    RadioBtnOpsi41: TRadioButton;
    RadioBtnOpsi42: TRadioButton;
    procedure BtnBackClick(Sender: TObject); override;
    procedure BtnNextClick(Sender: TObject); override;
    procedure FormActivate(Sender: TObject); override;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction); override;
    procedure RadioBtnOpsi41Change(Sender: TObject);
  end;

var
  Query: TQuery;

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query2;

{$R *.lfm}

{ TQuery }

procedure TQuery.BtnNextClick(Sender: TObject);
begin
  // Make Data entries:
  // opsi version
  if RadioBtnOpsi41.Checked then
    Data.opsiVersion := RadioBtnOpsi41.Caption
  else
    Data.opsiVersion := RadioBtnOpsi42.Caption;
  // Repository
  if RadioBtnRepo.Checked then
    Data.repo := EditDefaultRepo.Text
  else
    Data.repo := EditRepo.Text;
  // Proxy
  if RadioBtnNone.Checked then
    Data.proxy := ''
  else if RadioBtnMyProxy.Checked then
    Data.proxy := RadioBtnMyProxy.Caption
  else
    Data.proxy := EditProxy.Text;
  // Repository (no cache)
  if RadioBtnRepoNoCache.Checked then
    Data.repoNoCache := EditDefaultRepoNoCache.Text
  else
    Data.repoNoCache := EditOtherNoCache.Text;

  // show next form in custom setup
  showForm(Query2, self);
  Query2.BtnBack.Left := BtnBack.Left;
  Query2.BtnBack.Top := BtnBack.Top;
  Query2.BtnNext.Left := BtnNext.Left;
  Query2.BtnNext.Top := BtnNext.Top;
end;

procedure TQuery.FormActivate(Sender: TObject);
begin
  inherited FormActivate(Sender);

  // default opsi version is 4.2
  // set default repo depending on default opsi version
  if self.RadioBtnOpsi41.Checked then
    EditDefaultRepo.Text := Data.baseRepoUrlOpsi41
  else
    EditDefaultRepo.Text := Data.baseRepoUrlOpsi42;
  // same repo for no cache proxy
  EditDefaultRepoNoCache.Text := EditDefaultRepo.Text;

  // text by resourcestrings
  Caption := ProgramName + ' - ' + rsCapQuery;
  LabelOpsiVersion.Caption := rsOpsiVersion;
  RadioBtnOpsi41.Caption := rsOpsi41;
  RadioBtnOpsi42.Caption := rsOpsi42;
  //InfoOpsiVersion.Hint := rsInfoOpsiVersion;
  LabelRepo.Caption := rsRepo;
  RadioBtnOtherRepo.Caption := rsRepoOther;
  InfoRepo.Hint := rsInfoRepo;
  LabelProxy.Caption := rsProxy;
  RadioBtnNone.Caption := rsProxyNone;
  RadioBtnOtherProxy.Caption := rsProxyOther;
  LabelNoCache.Caption := rsRepoNoCache;
  RadioBtnOtherNoCache.Caption := rsRepoOther;
  BtnBack.Caption := rsBack;
  BtnNext.Caption := rsNext;
end;

procedure TQuery.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  QuickInstall.Close;
end;

procedure TQuery.RadioBtnOpsi41Change(Sender: TObject);
begin
  // when opsi version changes, adjust default repos
  if RadioBtnOpsi41.Checked then
  begin
    EditDefaultRepo.Text := Data.baseRepoUrlOpsi41;
    EditDefaultRepoNoCache.Text := EditDefaultRepo.Text;
  end
  else
  begin
    EditDefaultRepo.Text := Data.baseRepoUrlOpsi42;
    EditDefaultRepoNoCache.Text := EditDefaultRepo.Text;
  end;
end;

procedure TQuery.BtnBackClick(Sender: TObject);
begin
  showForm(QuickInstall, self);
end;

end.
