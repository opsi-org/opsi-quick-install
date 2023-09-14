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
    EditGrafanaRepo: TEdit;
    EditRepo: TEdit;
    EditOtherNoCache: TEdit;
    InfoRepo: TImage;
    InfoOpsiVersion: TImage;
    LabelNoCache: TLabel;
    LabelProxy: TLabel;
    LabelGrafanaRepo: TLabel;
    LabelRepo: TLabel;
    PanelNoCache: TPanel;
    PanelProxy: TPanel;
    PanelGrafanaRepo: TPanel;
    PanelRepo: TPanel;
    RadioBtnGrafanaRepoEnterprise: TRadioButton;
    RadioBtnGrafanaRepoOss: TRadioButton;
    RadioBtnOtherGrafanaRepo: TRadioButton;
    RadioBtnRepo: TRadioButton;
    RadioBtnNone: TRadioButton;
    RadioBtnOtherRepo: TRadioButton;
    RadioBtnOtherProxy: TRadioButton;
    RadioBtnOtherNoCache: TRadioButton;
    RadioBtnRepoNoCache: TRadioButton;
    RadioBtnMyProxy: TRadioButton;
    procedure BtnBackClick(Sender: TObject); override;
    procedure BtnNextClick(Sender: TObject); override;
    procedure FormActivate(Sender: TObject); override;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction); override;
  end;

var
  Query: TQuery;

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query2;

{$R *.lfm}

procedure TQuery.BtnNextClick(Sender: TObject);
begin
  // Make Data entries:
  // Repository
  if RadioBtnRepo.Checked then
    Data.repo := EditDefaultRepo.Text
  else
    Data.repo := EditRepo.Text;
  // opsi version
  if Pos('4.3', Data.repo) > 0 then
    Data.opsiVersion := 'opsi 4.3'
  else
    Data.opsiVersion := 'opsi 4.2';
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
  // Grafana repository
  if RadioBtnGrafanaRepoOss.Checked then
    Data.grafanaRepo := RadioBtnGrafanaRepoOss.Caption
  else if RadioBtnGrafanaRepoEnterprise.Checked then
    Data.grafanaRepo := RadioBtnGrafanaRepoEnterprise.Caption
  else
    Data.grafanaRepo := EditGrafanaRepo.Text;

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

  // default opsi version is 4.3
  // set default repo
  EditDefaultRepo.Text := Data.baseRepoUrlOpsi43;
  // same repo for no cache proxy
  EditDefaultRepoNoCache.Text := EditDefaultRepo.Text;

  // text by resourcestrings
  Caption := ProgramName + ' - ' + rsCapQuery;
  LabelRepo.Caption := rsRepo;
  RadioBtnOtherRepo.Caption := rsRepoOther;
  InfoRepo.Hint := rsInfoRepo;
  LabelProxy.Caption := rsProxy;
  RadioBtnNone.Caption := rsProxyNone;
  RadioBtnOtherProxy.Caption := rsProxyOther;
  LabelNoCache.Caption := rsRepoNoCache;
  RadioBtnOtherNoCache.Caption := rsRepoOther;
  LabelGrafanaRepo.Caption := rsGrafanaRepo;
  RadioBtnOtherGrafanaRepo.Caption := rsRepoOther;
  BtnBack.Caption := rsBack;
  BtnNext.Caption := rsNext;
end;

procedure TQuery.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  QuickInstall.Close;
end;

procedure TQuery.BtnBackClick(Sender: TObject);
begin
  showForm(QuickInstall, self);
end;

end.
