unit opsiquickinstall_QueryData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  oslog,
  opsi_quick_install_CommonResourceStrings,
  OpsiLinuxInstaller_QueryData;

type

  {TSplitData}
  // For data that appears in the overview and the file properties.conf in different ways,
  // e.g. OverviewEntry = 'Yes' or 'Ja' or 'Oui' or ... and PropertyEntry = 'true'
  TSplitData = class(TObject)
  private
    FOverviewEntry: string;
    FPropertyEntry: string; // mostly gets boolean value
  public
    procedure SetEntries(PropertyEntry: string);
    property OverviewEntry: string read FOverviewEntry;
    property PropertyEntry: string read FPropertyEntry;
  end;

  {TQuickInstallData}
  TQuickInstallData = class(TOpsiLinuxInstallerData)
  private
    procedure SetDefaultValues;
  public
  var
    CustomSetup: boolean;

    opsiVersion, repo, proxy, repoNoCache, grafanaRepo: string;

    copyMod: TSplitData;
    backend: string;
    repoKind: string;

    ucsPassword: string;
    reboot: TSplitData;
    dhcp: TSplitData;
    symlink: string;

    netmask, networkAddress, domain, nameserver, gateway: string;
    adminName, adminPassword, ipName, ipNumber: string;
  const
    baseRepoUrlOpsi43 =
      'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.3:/';
    baseRepoUrlOpsi42 =
      'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.2:/';

    constructor Create;
    destructor Destroy; override;
  end;

var
  Data: TQuickInstallData;

implementation

{TSplitData}

procedure TSplitData.SetEntries(PropertyEntry: string);
begin
  (*Language.TranslateProjectResourceStrings('opsi_quick_install_CommonResourceStrings',
    '../locale/opsi_quick_install_CommonResourceStrings.' +
    Language.Abbreviation + '.po');*)

  FPropertyEntry := PropertyEntry;
  if PropertyEntry = 'true' then
    FOverviewEntry := rsYes
  else
    FOverviewEntry := rsNo;
end;


{TQuickInstallData}

procedure TQuickInstallData.SetDefaultValues;
begin
  opsiVersion := 'opsi 4.3';
  repo := baseRepoUrlOpsi43;
  proxy := '';
  repoNoCache := repo;
  grafanaRepo := 'https://packages.grafana.com/oss';

  copyMod := TSplitData.Create;
  copyMod.SetEntries('false');
  backend := 'mysql';
  repoKind := 'testing';

  ucsPassword := '';
  reboot := TSplitData.Create;
  reboot.SetEntries('false');
  dhcp := TSplitData.Create;
  dhcp.SetEntries('false');
  symlink := 'default.nomenu';

  netmask := '';
  networkAddress := '';
  domain := '';
  nameserver := '';

  gateway := '';
  adminName := 'adminuser';
  adminPassword := 'linux123';
  ipName := 'auto';
  ipNumber := 'auto';
end;

constructor TQuickInstallData.Create;
begin
  inherited Create;
  SetDefaultValues;
end;

destructor TQuickInstallData.Destroy;
begin
  if Assigned(copyMod) then FreeAndNil(copyMod);
  if Assigned(reboot) then FreeAndNil(reboot);
  if Assigned(dhcp) then FreeAndNil(dhcp);
  inherited Destroy;
end;

end.
