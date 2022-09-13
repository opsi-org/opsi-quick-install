program opsi_quick_install_project;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  StrUtils,
  CustApp,
  Process,
  GetText,
  Translations,
  DistributionInfo,
  osRunCommandElevated,
  osfunclin,
  oslog,
  opsi_quick_install_resourcestrings,
  opsiquickinstall_data,
  osnetutil,
  opsi_quick_install_nogui_query,
  OpsiPackageDownloader,
  osfuncunix,
  SupportedOpsiServerDistributions,
  LogFileFunctions,
  opsiquickinstall_InstallationScriptExecuter,
  IndependentMessageDisplayer;

type
  TQuickInstall = class(TCustomApplication)
  private
    PropsFile: TStringList;
    QuickInstallCommand: TRunCommandElevated;
    procedure InstallOpsiServer;
    procedure NoGuiQuery;
    procedure ExecuteWithDefaultValues;
    procedure ReadPropertiesFromFile;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;


  {TQuickInstall}

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
            ReadPropertiesFromFile;
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


  procedure TQuickInstall.InstallOpsiServer;
  var
    LOpsiServerInstallationScriptExecuter: TLOpsiServerInstallationScriptExecuter;
    MessageDisplayer: TIndependentMessageDisplayer;
  begin
    MessageDisplayer := TIndependentMessageDisplayer.Create;
    LOpsiServerInstallationScriptExecuter :=
      TLOpsiServerInstallationScriptExecuter.Create('', False,
      Data.DistrInfo.PackageManagementShellCommand, 'l-opsi-server',
      'download.uib.de/opsi4.2/testing/packages/linux/localboot/', MessageDisplayer);

    writeln('');
    writeln(rsInstall + Data.opsiVersion + ':' + #10 + rsWait + LongMessageSeperator + rsSomeMin);
    LOpsiServerInstallationScriptExecuter.InstallOpsiProduct;
  end;


  procedure TQuickInstall.NoGuiQuery;
  var
    QuickInstallNoQuiQuery: TQuickInstallNoQuiQuery;
  begin
    QuickInstallNoQuiQuery := TQuickInstallNoQuiQuery.Create;
    // Start the series of queries and fill Data:
    QuickInstallNoQuiQuery.StartQuery;

    if QuickInstallNoQuiQuery.QueryFinished then
    begin
      FreeAndNil(QuickInstallNoQuiQuery);
      // After query:
      InstallOpsiServer;
    end;
  end;

  procedure TQuickInstall.ExecuteWithDefaultValues;
  begin
    LogDatei.log('Execute with default values:', LLdebug);
    InstallOpsiServer;
  end;

  procedure TQuickInstall.ReadPropertiesFromFile;
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

    InstallOpsiServer;
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

  procedure LogQuickInstallVersion;
  begin
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
        writeln(rsNoSupport + #10 + SupportedDistributionsInfoString);
        FreeAndNil(LogDatei);
        FreeAndNil(Data);
        Halt(1);
      end;
    end;
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
  CheckThatUserIsRoot;
  InitializeLogFile(LogfileName);
  LogQuickInstallVersion;

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

  CheckThatOqiSupportsDistribution(QuickInstall);
  QuickInstall.Run;

  QuickInstall.Free;

  writeln();
  LogDatei.Free;
end.
