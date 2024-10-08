program opsi_quick_install_project;

{$mode objfpc}{$H+}

uses
  Classes,
  SysUtils,
  StrUtils,
  CustApp,
  Process,
  DistributionInfo,
  osRunCommandElevated,
  osfunclin,
  oslog,
  opsiquickinstall_QueryData,
  osnetutil,
  opsi_quick_install_nogui_query,
  OpsiPackageDownloader,
  osfuncunix,
  SupportedOpsiServerDistributions,
  LogFileFunctions,
  opsiquickinstall_InstallationScriptExecuter,
  IndependentMessageDisplayer,
  opsi_quick_install_NoguiResourceStrings,
  opsi_quick_install_CommonResourceStrings,
  WelcomeResourceStrings,
  OpsiLinuxInstaller_LanguageObject;

type
  TQuickInstall = class(TCustomApplication)
  private
    PropsFile: TStringList;
    QuickInstallCommand: TRunCommandElevated;
    procedure InstallOpsiServer;
    procedure NoGuiQuery;
    procedure ReadPropertiesFromFile;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;


  procedure WelcomeUser;
  begin
    writeln('');
    writeln(StringReplace(rsWelcome, '[]', 'opsi-server', [rfReplaceAll]));
  end;

  procedure TranslateNoguiResourceStrings;
  begin
    Language.TranslateResourceStrings('opsi_quick_install_CommonResourceStrings',
      'opsi_quick_install_CommonResourceStrings.' + Language.Abbreviation + '.po');
    Language.TranslateResourceStrings('opsi_quick_install_NoguiResourceStrings',
      'opsi_quick_install_NoguiResourceStrings.' + Language.Abbreviation + '.po');
    Language.TranslateResourceStrings('WelcomeResourceStrings',
      'Welcome.' + Language.Abbreviation + '.po');
  end;

  procedure UseUserDefinedLanguage;
  var
    UserDefinedLang: string;
  begin
    writeln(rsSelLanguage, rsLangOp);
    readln(UserDefinedLang);
    // check for right input
    while not ((UserDefinedLang = 'de') or (UserDefinedLang = 'en') or
        (UserDefinedLang = 'fr') or (UserDefinedLang = 'es') or
        (UserDefinedLang = '')) do
    begin
      writeln('"', UserDefinedLang, '"', rsNotValid);
      readln(UserDefinedLang);
    end;

    // for UserDefinedLang = '' we keep the system language for which the resourcestrings were already translated
    if UserDefinedLang <> '' then
    begin
      Language.Abbreviation := UserDefinedLang;
      TranslateNoguiResourceStrings;
    end;

    writeln(rsCarryOut);
    sleep(50);
  end;


  {TQuickInstall}

  procedure TQuickInstall.DoRun;
  var
    ErrorMsg: string;
    FilePath: string;
    VersionFile: TStringList;
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
      WelcomeUser;
      UseUserDefinedLanguage;
      NoGuiQuery;
      Terminate;
      Exit;
    end;
    // no query, directly use all default values for installation
    if HasOption('d', 'default') then
    begin
      LogDatei.Log('Use default property values.', LLInfo);
      writeln('');
      VersionFile := TStringList.Create;
      VersionFile.LoadFromFile('../version.txt');
      VersionFile.NameValueSeparator := ':';
      writeln('Start ' + ProgramName + ' ' + VersionFile.Values['Mainversion'].Trim() +
        '-' + VersionFile.Values['Subversion'].Trim());
      FreeAndNil(VersionFile);
      InstallOpsiServer;
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
        writeln('Executing ' + ProgramName + ' with properties from file didn''t work!');
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
    LOpsiServerDownloadSite: string;
  begin
    MessageDisplayer := TIndependentMessageDisplayer.Create;
    if Data.opsiVersion = 'opsi 4.3' then
      LOpsiServerDownloadSite := 'opsipackages.43.opsi.org/stable/linux/localboot/'
    else
      LOpsiServerDownloadSite := 'download.uib.de/opsi4.2/stable/packages/linux/localboot/';
    LOpsiServerInstallationScriptExecuter :=
      TLOpsiServerInstallationScriptExecuter.Create('', False,
      Data.DistrInfo.PackageManagementShellCommand, 'l-opsi-server',
      LOpsiServerDownloadSite, MessageDisplayer);

    if HasOption('f', 'file') then
      LOpsiServerInstallationScriptExecuter.UsePropertiesFromFile(PropsFile);

    writeln('');
    LOpsiServerInstallationScriptExecuter.InstallOpsiProduct;
  end;


  procedure TQuickInstall.NoGuiQuery;
  var
    QuickInstallNoGuiQuery: TQuickInstallNoGuiQuery;
  begin
    QuickInstallNoGuiQuery := TQuickInstallNoGuiQuery.Create;
    // Start the series of queries and fill Data:
    QuickInstallNoGuiQuery.StartQuery;

    if QuickInstallNoGuiQuery.QueryFinished then
    begin
      FreeAndNil(QuickInstallNoGuiQuery);
      InstallOpsiServer;
    end;
  end;

  procedure TQuickInstall.ReadPropertiesFromFile;
  begin
    LogDatei.log('Read properties from file:', LLdebug);
    // Read from file what is required for adding the repo
    Data.repoKind := PropsFile.Values['repo_kind'];
    Data.repo := PropsFile.Values['opsi_online_repository'];

    // Read opsi version from repo url
    if Pos('4.3', Data.repo) > 0 then
      Data.opsiVersion := 'opsi 4.3'
    else
      Data.opsiVersion := 'opsi 4.2';

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
      writeln('Please execute ' + ProgramName + ' as root!');
      Halt(1);
    end;
  end;

  procedure LogQuickInstallVersion;
  var
    VersionFile: TStringList;
  begin
    LogDatei.log('', LLessential);
    VersionFile := TStringList.Create;
    VersionFile.LoadFromFile('../version.txt');
    VersionFile.NameValueSeparator := ':';
    LogDatei.log(ProgramName + ' version: ' + VersionFile.Values['Mainversion'].Trim() +
      '-' + VersionFile.Values['Subversion'].Trim(), LLessential);
    FreeAndNil(VersionFile);
    LogDatei.log('', LLessential);
  end;

  procedure UseSystemLanguageForResourcestrings;
  begin
    // get default language (system language)
    Language.Abbreviation := Copy(GetEnvironmentVariable('LANG'), 1, 2);
    if not MatchStr(Language.Abbreviation, ['de', 'en', 'fr', 'es']) then
      Language.Abbreviation := 'en';
    TranslateNoguiResourceStrings;
  end;

  procedure CheckThatOqiSupportsDistribution(QuickInstall: TQuickInstall);
  begin
    // In the query (option '-n') the checking of the distribution will be done later to
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

  Language := TLanguageObject.Create(
    '../../../lazarus/common/OpsiLinuxInstaller/locale/', '../locale/');
  UseSystemLanguageForResourcestrings;

  CheckThatOqiSupportsDistribution(QuickInstall);
  QuickInstall.Run;

  QuickInstall.Free;

  writeln();
  LogDatei.Free;
end.
