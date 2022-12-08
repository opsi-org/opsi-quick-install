unit opsi_quick_install_GuiResourceStrings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring

  rsCapDistr = 'Distribution';
  rsCapQuery = 'Query';
  rsCapQuery2 = 'Query (part 2)';
  rsCapQuery3 = 'Query (part 3)';
  rsCapQueryDhcp = 'Query (dhcp)';
  rsCapQueryUserInfo = 'Query (user info)';
  {Language}
  rsStandard = 'Standard';
  rsCustom = 'Custom';
  {Distribution}
  rsCorrect =
    'If this is correct, please click "Next" to continue. If not, please edit the distribution and then continue with "Next".';
  {Query}
  rsOpsiVersion = 'Which opsi version would you like to install?';
  rsOpsi41 = 'Opsi 4.1';
  rsOpsi42 = 'Opsi 4.2';
  rsRepoOther = 'Other:';
  rsProxy = 'Proxy to use (http://myproxy.dom.org:8080):';
  rsProxyNone = 'None';
  rsProxyOther = 'Other:';
  {Query5_dhcp}
  rsNetmaskOther = 'Other:';
  rsNetworkAddressOther = 'Other:';
  rsDomainOther = 'Other:';
  rsNameserverOther = 'Other:';
  rsGatewayOther = 'Other:';
  {Query6}
  rsOverviewBtn = ' overview ';
  rsFinish = ' Finish ';
  rsStartInstallation =
    'Finally click "Finish" to start the installation of opsi-server.';

implementation

end.
