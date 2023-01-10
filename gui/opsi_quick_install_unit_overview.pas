unit opsi_quick_install_unit_overview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  MaskEdit,
  OpsiLinuxInstaller_BaseForm,
  FormAppearanceFunctions,
  opsi_quick_install_CommonResourceStrings,
  opsi_quick_install_GuiResourceStrings,
  opsiquickinstall_QueryData,
  OpsiLinuxInstaller_LanguageObject;

type

  TOverview = class(TOpsiLinuxInstallerBaseForm)
    BtnBack: TButton;
    BtnFinish: TButton;
    LabelFinish: TLabel;
    MemoOverview: TMemo;
    PanelFinish: TPanel;
    procedure BtnBackClick(Sender: TObject);
    procedure BtnFinishClick(Sender: TObject);
    procedure FormActivate(Sender: TObject); override;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  end;

var
  Overview: TOverview;

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query6,
  opsi_quick_install_unit_password;

{$R *.lfm}

procedure TOverview.BtnFinishClick(Sender: TObject);
begin
  Overview.Enabled := False;
  Password.Visible := True;
end;

procedure TOverview.FormActivate(Sender: TObject);
begin
  inherited FormActivate(Sender);

  Language.TranslateResourceStrings('opsi_quick_install_CommonResourceStrings',
    'opsi_quick_install_CommonResourceStrings.' +
    Language.Abbreviation + '.po');

  MemoOverview.Left := QuickInstall.panelLeft;
  PanelFinish.Width := 400;

  MemoOverview.Clear;
  // Opsi version
  MemoOverview.Lines.Add(rsOpsiVersionOverview + Data.opsiVersion);

  {Custom installation}
  if Data.CustomSetup then
  begin
    MemoOverview.Lines.Add('');
    // Repository
    MemoOverview.Lines.Add(rsRepoOverview + Data.repo);
    // Proxy
    MemoOverview.Lines.Add(rsProxyOverview + Data.proxy);
    // Repository (no cache)
    MemoOverview.Lines.Add(rsRepoNoCacheOverview + Data.repoNoCache);
    // Grafana repository
    MemoOverview.Lines.Add(rsGrafanaRepoOverview + Data.grafanaRepo);

    MemoOverview.Lines.Add('');
    // Backend
    MemoOverview.Lines.Add(rsBackendOverview + Data.backend);
    // Copy modules
    MemoOverview.Lines.Add(rsCopyModulesOverview + Data.copyMod.OverviewEntry);
    // Repo kind
    MemoOverview.Lines.Add(rsRepoKindOverview + Data.repoKind);

    MemoOverview.Lines.Add('');
    // UCS password
    if lowerCase(Data.DistrInfo.DistroName) = 'univention' then
      MemoOverview.Lines.Add(rsUCSOverview + Data.ucsPassword);
    // Reboot
    MemoOverview.Lines.Add(rsRebootOverview + Data.reboot.OverviewEntry);
  end;

  {Both}
  // Dhcp
  if Data.dhcp.PropertyEntry = 'true' then
  begin
    MemoOverview.Lines.Add('');
    MemoOverview.Lines.Add(rsDhcpOverview + Data.dhcp.OverviewEntry);
    // TFTPROOT
    MemoOverview.Lines.Add(rsTFTPROOTOverview + Data.symlink);
    // Netmask
    MemoOverview.Lines.Add(rsNetmaskOverview + Data.netmask);
    // Network address
    MemoOverview.Lines.Add(rsNetworkOverview + Data.networkAddress);
    // Domain
    MemoOverview.Lines.Add(rsDomainOverview + Data.domain);
    // Nameservere
    MemoOverview.Lines.Add(rsNameserverOverview + Data.nameserver);
    // Gateway
    MemoOverview.Lines.Add(rsGatewayOverview + Data.gateway);
  end;

  MemoOverview.Lines.Add('');
  // Admin name
  MemoOverview.Lines.Add(rsAdminNameOverview + Data.adminName);
  // Admin password
  MemoOverview.Lines.Add(rsAdminPasswordOverview + Data.adminPassword);
  // IP name
  MemoOverview.Lines.Add(rsIPNameOverview + Data.ipName);
  // IP number
  MemoOverview.Lines.Add(rsIPNumberOverview + Data.ipNumber);

  // text by resourcestrings
  Caption := rsOverview;
  LabelFinish.Caption := rsStartInstallation;
  BtnBack.Caption := rsBack;
  BtnFinish.Caption := rsFinish;
end;

procedure TOverview.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  Query6.Close;
end;

procedure TOverview.BtnBackClick(Sender: TObject);
begin
  showForm(Query6, self);
end;

end.
