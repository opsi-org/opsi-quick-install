unit opsiquickinstall_data;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  DistributionInfo,
  osfunclin,
  oslog,
  opsi_quick_install_resourcestrings,
  OpsiLinuxInstaller_Data;

type

  {TSplitData}
  // For data that appears in the overview and the file properties.conf in different ways,
  // e.g. OverviewEntry = 'Yes' or 'Ja' or 'Oui' or ... and PropertyEntry = 'true'
  TSplitData = class(TObject)
  private
    FOverviewEntry: string;
    FPropertyEntry: string; // mostly gets boolean value
  public
    procedure SetEntries(SetOverviewEntry: string; SetPropertyEntry: string);
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

    opsiVersion, repo, proxy, repoNoCache: string;

    backend: string;
    copyMod: TSplitData;
    repoKind: string;

    ucsPassword: string;
    reboot: TSplitData;
    dhcp: TSplitData;
    symlink: string;

    netmask, networkAddress, domain, nameserver, gateway: string;
    adminName, adminPassword, ipName, ipNumber: string;
  const
    QuickInstallVersion = '4.2.0.8';
    baseRepoUrlOpsi41 =
      'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.1:/';
    baseRepoUrlOpsi42 =
      'http://download.opensuse.org/repositories/home:/uibmz:/opsi:/4.2:/';

    constructor Create;
    destructor Destroy; override;
  end;

var
  Data: TQuickInstallData;

implementation

{TSplitData}

procedure TSplitData.SetEntries(SetOverviewEntry: string; SetPropertyEntry: string);
begin
  FOverviewEntry := SetOverviewEntry;
  FPropertyEntry := SetPropertyEntry;
end;


{TQuickInstallData}

procedure TQuickInstallData.SetDefaultValues;
begin
  opsiVersion := 'Opsi 4.2';
  // automatically adjust repo to opsiVersion
  if opsiVersion = 'Opsi 4.2' then
    repo := baseRepoUrlOpsi42
  else
    repo := baseRepoUrlOpsi41;
  proxy := '';
  repoNoCache := repo;

  backend := 'file';
  copyMod := TSplitData.Create;
  copyMod.SetEntries(rsNo, 'false');
  repoKind := 'stable';

  ucsPassword := '';
  reboot := TSplitData.Create;
  reboot.SetEntries(rsNo, 'false');
  dhcp := TSplitData.Create;
  dhcp.SetEntries(rsNo, 'false');
  symlink := 'default.nomenu';

  netmask := '255.255.0.0';
  networkAddress := '192.168.0.0';
  domain := 'uib.local';
  nameserver := '192.168.1.245';

  gateway := '192.168.1.245';
  adminName := 'adminuser';
  adminPassword := 'linux123';
  ipName := 'auto';
  ipNumber := 'auto';
end;

constructor TQuickInstallData.Create;
begin
  inherited Create;

  // Following line takes time and is therefore executed only once at the
  // beginning of oqi when Data is created.
  DistrInfo := TDistributionInfo.Create(getLinuxDistroName, getLinuxDistroRelease);

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
