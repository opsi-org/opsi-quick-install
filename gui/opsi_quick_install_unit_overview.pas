unit opsi_quick_install_unit_overview;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  MaskEdit,
  OpsiLinuxInstaller_BaseForm,
  FormAppearanceFunctions,
  opsi_quick_install_resourcestrings,
  opsiquickinstall_QueryData;

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

  MemoOverview.Left := QuickInstall.panelLeft;
  PanelFinish.Width := 400;

  MemoOverview.Clear;
  // Opsi version
  MemoOverview.Lines.Add(rsOpsiVersionO + Data.opsiVersion);

  {Custom installation}
  if Data.CustomSetup then
  begin
    MemoOverview.Lines.Add('');
    // Repository
    MemoOverview.Lines.Add(rsRepoO + Data.repo);
    // Proxy
    MemoOverview.Lines.Add(rsProxyO + Data.proxy);
    // Repository (no cache)
    MemoOverview.Lines.Add(rsRepoNoCacheO + Data.repoNoCache);

    MemoOverview.Lines.Add('');
    // Backend
    MemoOverview.Lines.Add(rsBackendO + Data.backend);
    // Copy modules
    MemoOverview.Lines.Add(rsCopyModulesO + Data.copyMod.OverviewEntry);
    // Repo kind
    MemoOverview.Lines.Add(rsRepoKindO + Data.repoKind);

    MemoOverview.Lines.Add('');
    // UCS password
    if lowerCase(Data.DistrInfo.DistroName) = 'univention' then
      MemoOverview.Lines.Add(rsUCSO + Data.ucsPassword);
    // Reboot
    MemoOverview.Lines.Add(rsRebootO + Data.reboot.OverviewEntry);
  end;

  {Both}
  // Dhcp
  if Data.dhcp.PropertyEntry = 'true' then
  begin
    MemoOverview.Lines.Add('');
    MemoOverview.Lines.Add(rsDhcpO + Data.dhcp.OverviewEntry);
    // TFTPROOT
    MemoOverview.Lines.Add(rsTFTPROOTO + Data.symlink);
    // Netmask
    MemoOverview.Lines.Add(rsNetmaskO + Data.netmask);
    // Network address
    MemoOverview.Lines.Add(rsNetworkO + Data.networkAddress);
    // Domain
    MemoOverview.Lines.Add(rsDomainO + Data.domain);
    // Nameservere
    MemoOverview.Lines.Add(rsNameserverO + Data.nameserver);
    // Gateway
    MemoOverview.Lines.Add(rsGatewayO + Data.gateway);
  end;

  MemoOverview.Lines.Add('');
  // Admin name
  MemoOverview.Lines.Add(rsAdminNameO + Data.adminName);
  // Admin password
  MemoOverview.Lines.Add(rsAdminPasswordO + Data.adminPassword);
  // IP name
  MemoOverview.Lines.Add(rsIPNameO + Data.ipName);
  // IP number
  MemoOverview.Lines.Add(rsIPNumberO + Data.ipNumber);

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
