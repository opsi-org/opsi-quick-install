unit opsi_quick_install_CommonResourceStrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  LogOpsiServer = '/var/log/opsi-quick-install-l-opsi-server.log';
  {$IFDEF GUI}
  LongMessageSeperator = #10;
  {$ENDIF GUI}
  {$IFDEF NOGUI}
  LongMessageSeperator = '';
  {$ENDIF NOGUI}

resourcestring

  rsYes = 'Yes';
  rsNo = 'No';
  rsNext = ' Next ';
  rsBack = ' Back ';
  rsOverview = 'Overview';
  rsInvalidFqdnWarning =
    'Opsi-QuickInstall Warning:' + #10 +
    'We could not find a valid local DNS hostname on this computer.' +
    ' This can lead to problems during the installation! Please see' +
    #10 + 'https://docs.opsi.org/opsi-docs-en/4.2/quickinstall/' +
    LongMessageSeperator + 'quickinstall-manual.html#opsi-quickinstall-prep' +
    #10 + 'for more information.';

  {Queries}
  rsSetup = 'Please select a setup type:';
  rsDistr = 'We recognized the following distribution on this computer:';
  rsInfoDistribution = 'The following distributions are supported:';
  rsNoSupport =
    'Unfortunately, opsi does not support your distribution. Only the following distributions are supported:';
  rsRepo = '(Base-) Repository for the opsi-server installation:';
  rsInfoRepo = 'Source from which opsi will be installed.';
  rsRepoNoCache =
    '(Base-) Repository for the opsi-server installation (without any cache proxy):';
  rsBackend = 'Which backend should be installed? (mysql needs a valid activation file)';
  rsInfoBackend = 'Should the data of opsi be stored in a file or a mysql database?';
  rsCopyModules = 'Should we copy modules even if /etc/opsi/modules still exists?';
  rsInfoModules =
    'For mysql you need active modules. For further information and prices please write an e-mail to info@uib.de';
  rsRepoKind = 'From which repo kind should be installed?';
  rsInfoRepoKind = 'Select the stability of the installation:' + #10 +
    'Stable is reliable, testing is the beta version, experimental may cause some errors.';
  rsUCS =
    'What is the password of the administrator of the UCS domain controller (needed for other roles to join)?';
  rsReboot = 'May the server reboot if this script is finished?';
  rsInfoReboot =
    'If yes, shortly after the opsi-server is installed, your computer will reboot.';
  rsDhcp = 'Should we run the opsi dhcp-server on this machine?';
  rsInfoDhcp = 'Select "No" if you already have a dhcp-server.' +
    #10 + 'Select "Yes" if you want to install the dhcp-server on the opsi-server.';
  rsTFTPROOT = 'Which file should the TFTPROOT default symlink point to?';
  rsInfoTFTPROOT =
    'Choose "default.menu" if you want to show a small menu when no netboot product is on setup (bios only).';
  rsInfoNetwork = 'Information on the network in which the opsi-server will be installed.';
  rsNetmask = 'Netmask (for dhcp):';
  rsNetworkAddress = 'Network address (for dhcp):';
  rsDomain = 'DNS domain (for dhcp):';
  rsNameserver = 'Primary nameserver (for dhcp):';
  rsGateway = 'Gateway (option routers for dhcp):';
  rsAdminName = 'What is the name of the opsi admin user to create? (empty = nothing created)';
  rsInfoAdmin = 'User to create for the operation of opsi.' + #10 +
    'This user can not manage system settings.';
  rsAdminPassword =
    'What is the password of the opsi admin user to create? (empty = nothing created)';
  rsIPName =
    'Set a different IP name ("auto" = use automatically determined FQDN of this computer)';
  rsIPNumber =
    'Set a different IP number ("auto" = use automatically determined IP address of this computer)';

  {Overview}
  rsOpsiVersionOverview = 'Opsi version to install: ';
  rsRepoOverview = 'Repository: ';
  rsProxyOverview = 'Proxy: ';
  rsRepoNoCacheOverview = 'Repository (without cache proxy): ';
  rsBackendOverview = 'Backend: ';
  rsCopyModulesOverview = 'Copy modules: ';
  rsRepoKindOverview = 'Repo kind: ';
  rsUCSOverview = 'Password of administrator of UCS domain controller: ';
  rsRebootOverview = 'Reboot after script is finished: ';
  rsDhcpOverview = 'Run opsi dhcp-server: ';
  rsTFTPROOTOverview = 'TFTPROOT symlink points to: ';
  rsNetmaskOverview = 'Netmask: ';
  rsNetworkOverview = 'Network address: ';
  rsDomainOverview = 'DNS domain: ';
  rsNameserverOverview = 'Primary nameserver: ';
  rsGatewayOverview = 'Gateway: ';
  rsAdminNameOverview = 'Opsi admin user name: ';
  rsAdminPasswordOverview = 'Opsi admin user password: ';
  rsIPNameOverview = 'IP name: ';
  rsIPNumberOverview = 'IP number: ';

implementation

end.
