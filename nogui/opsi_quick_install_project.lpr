program opsi_quick_install_project;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
  {$IFDEF UseCThreads}
  cthreads,
    {$ENDIF}
    {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  Process,
  GetText,
  Translations,
  DistributionInfo,
  osRunCommandElevated,
  osfunclin,
  LinuxRepository,
  oslog,
  osnetworkcalculator,
  opsi_quick_install_resourcestrings,
  opsiquickinstall_data,
  osnetutil,
  opsi_quick_install_nogui_query,
  OpsiPackageDownloader,
  osfuncunix;

type
  TQuickInstall = class(TCustomApplication)
  private
  var
    FileText, PropsFile: TStringList;
    QuickInstallCommand: TRunCommandElevated;
    DirClientData, Output: string;
    two_los_to_test, one_installation_failed: boolean;
    name_los_default, name_los_downloaded, name_current_los: string;
    version_los_default, version_los_downloaded: string;
  const
    procedure SetDefaultValues;
    procedure defineDirClientData;
    procedure writePropsToFile;
    procedure addRepo;
    procedure executeLOSscript;
    procedure installOpsi;
    procedure NoGuiQuery;

    procedure ExecuteWithDefaultValues;
    procedure ReadPropsFromFile;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;


  { TQuickInstall }

  procedure TQuickInstall.DoRun;
  var
    ErrorMsg: string;
    FilePath: string;
  begin
    // quick check parameters
    ErrorMsg := CheckOptions('hgndf:', 'help gui nogui default file:');
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;
    // parse parameters:
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;
    // out of use
    if HasOption('g', 'gui') then
    begin
      // don't call gui version
      (*project := ExtractFilePath(ParamStr(0));
      Delete(project, Length(project), 1);
      project := ExtractFilePath(project) + 'gui/opsi_quick_install_project';
      writeln(project);
      ExecuteProcess(project, '', []);*)
      Terminate;
      Exit;
    end;
    // query
    if HasOption('n', 'nogui') then
    begin
      LogDatei.Log('Get properties from query.', LLInfo);
      NoGuiQuery;
      Terminate;
      Exit;
    end;
    // no query, directly use all default values for installation
    if HasOption('d', 'default') then
    begin
      LogDatei.Log('Use default properties.', LLInfo);
      ExecuteWithDefaultValues;
      Terminate;
      Exit;
    end;
    // no query, read in values from a file
    if HasOption('f', 'file') then
    begin
      // read properties from file
      PropsFile := TStringList.Create;
      FilePath := getOptionValue('f', 'file');
      LogDatei.Log('Get properties from file.', LLInfo);
      try
        begin
          if FileExists(FilePath) then
          begin
            PropsFile.LoadFromFile(FilePath);
            ReadPropsFromFile;
          end
          else
          begin
            writeln('File "' + FilePath + '" not found!');
          end;
        end;
      except
        writeln('Executing Opsi-QuickInstall with properties from file didn''t work!');
      end;
      PropsFile.Free;
      Terminate;
      Exit;
    end;

    // stop program loop
    Terminate;
    Exit;
  end;

  constructor TQuickInstall.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TQuickInstall.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TQuickInstall.WriteHelp;
  begin
    { add your help code here }
    writeln('-d [-default]     : Use the default values for the opsi-server installation and immmediately start the installation.');
    writeln('-f [-file] <file> : Use the values from a file for the opsi-server installation and immmediately start the installation.');
    writeln('-h [-help]        : See this information.');
    writeln('-n [-nogui]       : Start a setup program, in which you can set the values for the installation seperately');
  end;

  // set default values for all variables that are required for the installation
  procedure TQuickInstall.SetDefaultValues;
  begin
    LogDatei.log('Set default values', LLdebug);
    Data.opsiVersion := 'Opsi 4.2';
    if Data.opsiVersion = 'Opsi 4.1' then
      Data.repo := Data.baseRepoUrlOpsi41
    else
      Data.repo := Data.baseRepoUrlOpsi42;
    Data.proxy := '';
    Data.repoNoCache := Data.repo;
    Data.backend := 'file';
    Data.copyMod.SetEntries(rsNo, 'false');
    Data.repoKind := 'stable';
    Data.ucsPassword := '';
    Data.reboot.SetEntries(rsNo, 'false');
    Data.dhcp.SetEntries(rsNo, 'false');
    Data.symlink := 'default.nomenu';
    Data.netmask := '255.255.0.0';
    Data.networkAddress := '192.168.0.0';
    Data.domain := 'uib.local';
    Data.nameserver := '192.168.1.245';
    Data.gateway := '192.168.1.245';
    Data.adminName := 'adminuser';
    Data.adminPassword := 'linux123';
    Data.ipName := 'auto';
    Data.ipNumber := 'auto';
  end;

  procedure TQuickInstall.DefineDirClientData;
  var
    los_default_search, los_downloaded_search: TSearchRec;
  begin
    DirClientData := ExtractFilePath(ParamStr(0));
    Delete(DirClientData, Length(DirClientData), 1);
    //DirClientData := ExtractFilePath(DirClientData) + 'l-opsi-server';
    DirClientData := ExtractFilePath(DirClientData);

    if two_los_to_test then writeln(rsWait);
    // try downloading latest l-opsi-server and set DirClientData for the latest version
    if two_los_to_test and DownloadOpsiPackage('l-opsi-server',
      'download.uib.de/opsi4.2/testing/packages/linux/localboot/',
      QuickInstallCommand, Data.DistrInfo) then
    begin
      // extract and compare version numbers of default and downloaded los
      if (FindFirst('../l-opsi-server_4.*', faAnyFile and faDirectory,
        los_default_search) = 0) and
        (FindFirst('../downloaded_l-opsi-server_4.*', faAnyFile and
        faDirectory, los_downloaded_search) = 0) then
      begin
        name_los_default := los_default_search.Name;
        name_los_downloaded := los_downloaded_search.Name;
        // extract version numbers
        version_los_default := los_default_search.Name;
        Delete(version_los_default, 1, Pos('_', version_los_default));
        version_los_downloaded := los_downloaded_search.Name;
        Delete(version_los_downloaded, 1, Pos('_', version_los_downloaded));
        Delete(version_los_downloaded, 1, Pos('_', version_los_downloaded));
        // compare and use latest l-opsi-server version
        if version_los_downloaded > version_los_default then
          name_current_los := name_los_downloaded
        else
        begin
          name_current_los := name_los_default;
          if version_los_downloaded = version_los_default then
            two_los_to_test := False;
        end;
      end;
    end
    else
    if one_installation_failed then
    begin
      // if there is a downloaded los but the latest los version failed to install,
      // switch between name_los_default and name_los_downloaded to get the dir of
      // the older version
      if version_los_downloaded > version_los_default then
        name_current_los := name_los_default
      else
        name_current_los := name_los_downloaded;
    end
    else
    // otherwise, in the case that downloading the latest l-opsi-server failed,
    // use the default one
    if FindFirst('../l-opsi-server_4.*', faAnyFile and faDirectory,
      los_default_search) = 0 then
    begin
      name_los_default := los_default_search.Name;
      // extract version numbers
      version_los_default := los_default_search.Name;
      Delete(version_los_default, 1, Pos('_', version_los_default));
      name_current_los := name_los_default;
      two_los_to_test := False;
    end;
    DirClientData += name_current_los + '/CLIENT_DATA/';
  end;

  // write properties in properties.conf file
  procedure TQuickInstall.WritePropsToFile;
  begin
    LogDatei.log('Entered WritePropsToFile', LLdebug);
    // write file text
    FileText := TStringList.Create;

    FileText.Add('allow_reboot=' + Data.reboot.PropertyEntry);
    FileText.Add('backend=' + Data.backend);
    FileText.Add('dnsdomain=' + Data.domain);
    FileText.Add('force_copy_modules=' + Data.copyMod.PropertyEntry);
    FileText.Add('gateway=' + Data.gateway);
    FileText.Add('install_and_configure_dhcp=' + Data.dhcp.PropertyEntry);
    FileText.Add('myipname=' + Data.ipName);
    FileText.Add('myipnumber=' + Data.ipNumber);
    FileText.Add('nameserver=' + Data.nameserver);
    FileText.Add('netmask=' + Data.netmask);
    FileText.Add('network=' + Data.networkAddress);
    FileText.Add('opsi_admin_user_name=' + Data.adminName);
    FileText.Add('opsi_admin_user_password=' + Data.adminPassword);
    FileText.Add('opsi_online_repository=' + Data.repo);
    FileText.Add('opsi_noproxy_online_repository=' + Data.repoNoCache);
    FileText.Add('patch_default_link_for_bootimage=' + Data.symlink);
    FileText.Add('proxy=' + Data.proxy);
    FileText.Add('repo_kind=' + Data.repoKind);
    FileText.Add('ucs_master_admin_password=' + Data.ucsPassword);
    // update_test shall always be false
    FileText.Add('update_test=false');

    DefineDirClientData;

    // write in properties.conf file:
    if not FileExists(DirClientData + 'properties.conf') then
      QuickInstallCommand.Run('touch ' + DirClientData + 'properties.conf', Output);
    QuickInstallCommand.Run('chown -c $USER ' + DirClientData +
      'properties.conf', Output);
    FileText.SaveToFile(DirClientData + 'properties.conf');

    FileText.Free;
  end;

  procedure TQuickInstall.AddRepo;
  var
    url: string;
    ReleaseKeyRepo: TLinuxRepository;
  begin
    writeln(rsCreateRepo);
    // first remove opsi.list to have a cleared opsi repository list
    if FileExists('/etc/apt/sources.list.d/opsi.list') then
      QuickInstallCommand.Run('rm /etc/apt/sources.list.d/opsi.list', Output);
    // create repository (no password, user is root):
    ReleaseKeyRepo := TLinuxRepository.Create(Data.DistrInfo.Distr, '', False);
    // set OpsiVersion and OpsiBranch afterwards using GetDefaultURL
    if Data.opsiVersion = 'Opsi 4.1' then
      ReleaseKeyRepo.GetDefaultURL(Opsi41, stringToOpsiBranch(Data.repoKind))
    else
      ReleaseKeyRepo.GetDefaultURL(Opsi42, stringToOpsiBranch(Data.repoKind));
    // define repo url
    url := Data.repo + Data.repoKind + '/' + Data.DistrInfo.DistrRepoUrlPart;

    // !following lines need an existing LogDatei
    if (Data.DistrInfo.DistroName = 'openSUSE') or
      (Data.DistrInfo.DistroName = 'SUSE') then
    begin
      writeln('OpenSUSE/SUSE: Add Repo');
      ReleaseKeyRepo.AddSuseRepo(url, 'OpsiQuickInstallRepositoryNew');
    end
    else
      ReleaseKeyRepo.Add(url);

    ReleaseKeyRepo.Free;
  end;

  // install opsi-script and execute l-opsi-server script
  procedure TQuickInstall.ExecuteLOSscript;
  begin
    // Set text of result.conf to 'failed' first (for safety)
    FileText := TStringList.Create;
    FileText.Add('failed');
    if not FileExists(DirClientData + 'result.conf') then
      QuickInstallCommand.Run('touch ' + DirClientData + 'result.conf', Output);

    QuickInstallCommand.Run('chown -c $USER ' + DirClientData + 'result.conf', Output);
    FileText.SaveToFile(DirClientData + 'result.conf');
    FileText.Free;

    Data.DistrInfo.SetPackageManagementShellCommand;
    // !following lines need an existing LogDatei
    // if one installation failed, then opsi-script was already installed
    if not one_installation_failed then
    begin
      QuickInstallCommand.Run(Data.DistrInfo.PackageManagementShellCommand +
        'update', Output);
      writeln(rsInstall + 'opsi-script...');
      QuickInstallCommand.Run(Data.DistrInfo.PackageManagementShellCommand +
        'install opsi-script', Output);
    end;
    //Output := InstallOpsiCommand.Run('opsi-script -silent -version');
    //writeln(Output);
    // remove the QuickInstall repo entry because it was only for installing opsi-script
    if FileExists('/etc/apt/sources.list.d/opsi.list') then
      QuickInstallCommand.Run('rm /etc/apt/sources.list.d/opsi.list', Output);

    writeln(rsInstall + name_current_los + '... ' + rsSomeMin);
    // "opsi-script -batch" for installation with gui window,
    // "opsi-script-nogui -batch" for without?
    // new: opsi-script -silent for nogui
    QuickInstallCommand.Run('opsi-script -silent -batch ' + DirClientData +
      'setup.opsiscript /var/log/opsi-quick-install-l-opsi-server.log', Output);
  end;

  // install opsi-server
  // requires: opsiVersion, repoKind, distroName, DistrInfo, existing LogDatei
  procedure TQuickInstall.InstallOpsi;
  var
    installationResult: string;
  begin
    LogDatei.log('Entered InstallOpsi', LLdebug);
    writeln('');
    writeln(rsInstall + Data.opsiVersion + ':');
    addRepo;

    // install opsi-server
    two_los_to_test := True;
    one_installation_failed := False;
    if HasOption('f', 'file') then
    begin
      defineDirClientData;
      // take text of PropsFile as text for properties.conf
      PropsFile.SaveToFile(DirClientData + 'properties.conf');
    end
    else
      writePropsToFile;

    executeLOSscript;

    // get result from result file and print it
    FileText := TStringList.Create;
    FileText.LoadFromFile(DirClientData + 'result.conf');
    // adjust quick-install ExitCode
    if (FileText[0] = 'failed') and two_los_to_test then
    begin
      // if installation of latest l-opsi-server failed, try the older version:
      writeln(rsInstallation + rsFailed + '. ' + rsTryOlderLOS + '.');
      Sleep(1000);
      LogDatei.log('Installation failed: ' + name_current_los, LLessential);
      LogDatei.log('Try older version of l-opsi-server:', LLnotice);
      two_los_to_test := False;
      one_installation_failed := True;
      FileText.Free;
      if HasOption('f', 'file') then
      begin
        DefineDirClientData;
        // take text of PropsFile as text for properties.conf
        PropsFile.SaveToFile(DirClientData + 'properties.conf');
      end
      else
        WritePropsToFile;

      executeLOSscript;
      FileText := TStringList.Create;
      FileText.LoadFromFile(DirClientData + 'result.conf');
    end;

    if FileText[0] = 'failed' then
    begin
      installationResult := rsFailed;
      writeln(rsInstallation + rsFailed + '.');
      LogDatei.log('Installation failed: ' + name_current_los, LLessential);
      LogDatei.log(Data.opsiVersion + ' installation failed', LLessential);
      ExitCode := 1;
    end
    else
    begin
      installationResult := rsSuccess;
      LogDatei.log('Installation successful: ' + name_current_los, LLessential);
      LogDatei.log(Data.opsiVersion + ' installation successful', LLessential);
    end;
    // print result of installation
    Sleep(1000);
    writeln();
    writeln(rsInstallationOf + Data.opsiVersion + ' ' + installationResult + '!');
    Sleep(1000);
    writeln();
    writeln(rsLog);
    writeln(LogOpsiServer);

    QuickInstallCommand.Free;
    FileText.Free;
  end;

  procedure TQuickInstall.NoGuiQuery;
  var
    QuickInstallNoQuiQuery: TQuickInstallNoQuiQuery;
  begin
    SetDefaultValues;

    QuickInstallNoQuiQuery := TQuickInstallNoQuiQuery.Create;
    // Start the series of queries and fill Data:
    QuickInstallNoQuiQuery.QueryDistribution;

    if QuickInstallNoQuiQuery.QueryFinished then
    begin
      FreeAndNil(QuickInstallNoQuiQuery);
      // After query:
      InstallOpsi;
    end;
  end;


  // no query options:

  procedure TQuickInstall.ExecuteWithDefaultValues;
  begin
    LogDatei.log('Execute with default values:', LLdebug);
    SetDefaultValues;
    InstallOpsi;
  end;

  procedure TQuickInstall.ReadPropsFromFile;
  begin
    LogDatei.log('Read properties from file:', LLdebug);
    // Read from file what is required for adding the repo
    Data.repoKind := PropsFile.Values['repo_kind'];
    Data.repo := PropsFile.Values['opsi_online_repository'];

    // Read opsi version from repo url
    if Pos('4.1', Data.repo) > 0 then
      Data.opsiVersion := 'Opsi 4.1'
    else
      Data.opsiVersion := 'Opsi 4.2';

    InstallOpsi;
  end;

  {Program}

  procedure CheckThatUserIsRoot;
  var
    user, userID: string;
  begin
    // get user and as safeguard also the user id
    if (RunCommand('/bin/sh', ['-c', 'echo | id -nu'], user) and
      RunCommand('/bin/sh', ['-c', 'echo | id -u'], userID)) then
    begin
      Delete(user, user.Length, 1);
      Delete(userID, userID.Length, 1);
    end;

    if not ((user = 'root') and (userID = '0')) then
    begin
      writeln('Please execute Opsi Quick-Install as root!');
      Halt(1);
    end;
  end;

  procedure InitializeLogfile(LogfileName: string);
  begin
    // log file will be created in /tmp/opsi_quickinstall.log
    LogDatei := TLogInfo.Create;
    LogDatei.CreateTheLogfile(LogfileName);
    LogDatei.log('Log file created', LLdebug);
    SetCurrentDir(ExtractFilePath(ParamStr(0)));
    LogDatei.log('Working directory: ' + GetCurrentDir, LLessential);
    LogDatei.log('', LLessential);
    LogDatei.log('Opsi-QuickInstall version: ' + Data.QuickInstallVersion, LLessential);
    LogDatei.log('', LLessential);
  end;

  procedure UseSystemLanguageForResourcestrings;
  var
    Lang, DefLang: string;
  begin
    // get default language (system language)
    GetLanguageIDs(Lang, DefLang);
    // use po-files of gui version (because LCL (from the gui version) does not
    // seem to be able to use po-files from other directories while the nogui
    // version is flexible)
    TranslateUnitResourceStrings('opsi_quick_install_resourcestrings',
      '../gui/locale/opsi_quick_install_project.%s.po', Lang, DefLang);
  end;

  procedure WelcomeUser;
  begin
    writeln('');
    writeln(rsWelcome);
  end;

  procedure UseUserDefinedLanguage;
  var
    UserDefinedLang: string;
  begin
    writeln(rsSelLanguage, rsLangOp);
    readln(UserDefinedLang);
    // check for right input
    while not ((UserDefinedLang = 'de') or (UserDefinedLang = 'en') or
        (UserDefinedLang = 'fr') or (UserDefinedLang = 'es')) do
    begin
      writeln('"', UserDefinedLang, '"', rsNotValid);
      readln(UserDefinedLang);
    end;
    TranslateUnitResourceStrings('opsi_quick_install_resourcestrings',
      '../gui/locale/opsi_quick_install_project.' + UserDefinedLang + '.po');

    writeln(rsCarryOut);
    sleep(50);
  end;

  procedure CheckThatOqiSupportsDistribution(QuickInstall: TQuickInstall);
  begin
    // In the nogui query the checking of the distribution will be done later to
    // give the user the option to edit a wrongly detected distribution.
    if QuickInstall.HasOption('d', 'default') or QuickInstall.HasOption('f', 'file') then
    begin
      if Data.DistrInfo.Distr = other then
      begin
        writeln(rsNoSupport + #10 + Data.DistrInfo.Distribs);
        FreeAndNil(LogDatei);
        FreeAndNil(Data);
        Halt(1);
      end;
    end;
  end;

  procedure InitializeDistributionInfo(QuickInstall: TQuickInstall);
  begin
    Data.DistrInfo := TDistributionInfo.Create(getLinuxDistroName,
      getLinuxDistroRelease);
    LogDatei.log(Data.DistrInfo.DistroName + ' ' + Data.DistrInfo.DistroRelease,
      LLessential);
    Data.DistrInfo.SetDistrAndUrlPart;
    CheckThatOqiSupportsDistribution(QuickInstall);
  end;

  procedure CheckFQDN;
  begin
    if not isValidFQDN(GetFQDNUnix) then
    begin
      Writeln('');
      Writeln(rsInvalidFqdnWarning);
    end;
  end;

var
  QuickInstall: TQuickInstall;
  //r: TTranslateUnitResult;
const
  LogfileName = 'opsi_quickinstall_nogui.log';

{$R *.res}

begin
  // Only execute Opsi-QuickInstall(oqi) if user is root
  CheckThatUserIsRoot;
  InitializeLogfile(LogfileName);

  QuickInstall := TQuickInstall.Create(nil);
  Data := TQuickInstallData.Create;
  QuickInstall.QuickInstallCommand := TRunCommandElevated.Create('', False);
  CheckFQDN;

  UseSystemLanguageForResourcestrings;
  // do language selection here only for nogui installation
  if QuickInstall.HasOption('n', 'nogui') then
  begin
    WelcomeUser;
    UseUserDefinedLanguage;
  end;

  // Indicate start of program for default installation (also good
  // for test environment log to identify start of QuickInstall).
  if QuickInstall.HasOption('d', 'default') then
  begin
    writeln('');
    writeln('Start Opsi-QuickInstall ' + Data.QuickInstallVersion);
  end;

  InitializeDistributionInfo(QuickInstall);
  QuickInstall.Run;

  QuickInstall.Free;

  writeln(LogDatei.StandardMainLogPath + logFileName);
  writeln();
  LogDatei.Free;
end.
