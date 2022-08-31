unit opsi_quick_install_unit_language;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, LCLtranslator, Buttons, process,
  osnetutil,
  osfuncunix,
  OpsiLinuxInstaller_WelcomeForm,
  FormAppearanceFunctions,
  LogFileFunctions,
  oslog;

type

  TQuickInstall = class(TOpsiLinuxInstallerWelcomeForm)
    LabelSetup: TLabel;
    QuickInstallPanel: TPanel;
    RadioBtnDefault: TRadioButton;
    RadioBtnCustom: TRadioButton;
    procedure RemoveFuzziesFromLocaleFiles;
    procedure SetDefaultLanguage(const Languages: TStringList);
    procedure FillLanguageSelection;
    procedure GetBtnFinishWidth;
    procedure BtnNextClick(Sender: TObject); override;
    procedure ComboBoxLanguagesChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
  public
  const
    LogFileName = 'opsi_quickinstall.log';
  var
    // same position for all buttons
    BtnNextWidth, BtnFinishWidth: integer;
    // Set position for BtnNext on first form !manually! for right position
    // after language change.
    // needs to be done for each language!
    procedure SetBtnWidth(Language: string);
  end;


var
  QuickInstall: TQuickInstall;

implementation

uses
  opsi_quick_install_resourcestrings,
  opsiquickinstall_data,
  opsi_quick_install_unit_distr,
  opsi_quick_install_unit_query,
  opsi_quick_install_unit_query4;

{$R *.lfm}

procedure TQuickInstall.RemoveFuzziesFromLocaleFiles;
var
  removeFuzzys: string;
begin
  // from all po files remove all fuzzys that might have been introduced by the nogui version
  RunCommand('/bin/sh', ['-c',
    'echo | msgattrib --clear-fuzzy -o ../gui/locale/opsi_quick_install_project.de.po ../gui/locale/opsi_quick_install_project.de.po'],
    removeFuzzys);
  RunCommand('/bin/sh', ['-c',
    'echo | msgattrib --clear-fuzzy -o ../gui/locale/opsi_quick_install_project.en.po ../gui/locale/opsi_quick_install_project.en.po'],
    removeFuzzys);
  RunCommand('/bin/sh', ['-c',
    'echo | msgattrib --clear-fuzzy -o ../gui/locale/opsi_quick_install_project.es.po ../gui/locale/opsi_quick_install_project.es.po'],
    removeFuzzys);
  RunCommand('/bin/sh', ['-c',
    'echo | msgattrib --clear-fuzzy -o ../gui/locale/opsi_quick_install_project.fr.po ../gui/locale/opsi_quick_install_project.fr.po'],
    removeFuzzys);
end;

// Set position for BtnNext on first form !manually! for right position after
// language change.
// needs to be done for each language!
procedure TQuickInstall.SetBtnWidth(Language: string);
begin
  // width of BtnNext needs to be hard coded for every language for nice placement of BtnNext
  if Language = 'de' then
    BtnNextWidth := 63
  else if Language = 'en' then
    BtnNextWidth := 51
  else if Language = 'es' then
    BtnNextWidth := 80
  else if Language = 'fr' then
    BtnNextWidth := 68;
  //note that BtnNext.Width = width for english caption and
  //BtnNext.Left := Width - BtnNext.Width - BtnBack.Left; doesn't help either
  BtnNext.Left := Width - BtnBack.Left - BtnNextWidth;
end;

procedure TQuickInstall.SetDefaultLanguage(const Languages: TStringList);
begin
  // let the combo box show the system language at the beginning
  ComboBoxLanguages.ItemIndex := Languages.IndexOf(GetDefaultLang);
  // now set position of BtnNext for the default language
  SetBtnWidth(GetDefaultLang);
end;

procedure TQuickInstall.FillLanguageSelection;
var
  Languages: TStringList;
begin
  ComboBoxLanguages.Left := Round((WelcomePanel.Width - ComboBoxLanguages.Width) / 2);
  Languages := TStringList.Create;
  Languages.Add('de');
  Languages.Add('en');
  Languages.Add('es');
  Languages.Add('fr');
  SetDefaultLanguage(Languages);
end;

procedure TQuickInstall.FormCreate(Sender: TObject);
begin
  inherited FormCreate(Sender);
  CenterFormOnScreen(Sender as TForm);

  RemoveFuzziesFromLocaleFiles;

  // set constant button positions:
  BtnBack.Left := 20;
  //note that BtnNext.Width = width for english caption
  BtnNext.Left := Width - BtnBack.Left - BtnNext.Width;
  //BtnBack.Top := 410;
  BtnBack.Top := Height - 50;
  BtnNext.Top := BtnBack.Top;

  with ComboBoxLanguages.Items do
  begin
    Add('Deutsch');
    Add('English');
    Add('Español');
    Add('Français');
  end;
  FillLanguageSelection;

  InitializeLogFile(LogFileName);
  LogDatei.log('', LLnothing);
  LogDatei.log('Opsi-QuickInstall version: ' + Data.QuickInstallVersion, LLessential);
  LogDatei.log('', LLnothing);

  // initialize data structure to store the QuickInstall data for easier access
  Data := TQuickInstallData.Create;

  // text by resourcestrings
  LabelWelcome.Caption := rsWelcome;
  LabelSelLanguage.Caption := rsSelLanguage;
  LabelSetup.Caption := rsSetup;
  RadioBtnDefault.Caption := rsStandard;
  RadioBtnCustom.Caption := rsCustom;
  LabelCarryOut.Caption := rsCarryOut;
  BtnNext.Caption := rsNext;
end;

procedure TQuickInstall.GetBtnFinishWidth;
begin
  // Get width of BtnFinish for TPassword
  // through invisible buttons in WelcomeForm:
  // This is necessary because the positioning of the buttons does not work
  // properly on FormActivate in the respective forms (same problem as
  // here in TQuickInstall with BtnNext).
  // Btn.Caption:=rsString and Btn.Width only work properly when Btn.Visible=True
  BtnFinish.Visible := True;
  BtnFinish.Caption := rsFinish;
  BtnFinishWidth := BtnFinish.Width;
  BtnFinish.Visible := False;

  // Get width of BtnNext here once for procedure SetBtnWidth
  //ShowMessage(BtnNext.Width.ToString);
end;

procedure TQuickInstall.BtnNextClick(Sender: TObject);
begin
  // store in Data whether we are in custom installation or not
  if RadioBtnCustom.Checked then
    Data.CustomSetup := True
  else
    Data.CustomSetup := False;

  // before going on, let the user check the distribution
  Distribution.ShowModal;

  GetBtnFinishWidth;

  // Distribution.GoOn tells TQuickInstall whether in TDistribution the next or
  // the back button was clicked, i.e. whether to go on to the next form or to
  // stay on TQuickInstall after TDistribution closed.
  if Distribution.GoOn then
  begin
    // in standard setup go on to TQuery4
    if not Data.CustomSetup then
    begin
      // 'self' is current form
      showForm(Query4, self);
      // for having the buttons always at the same place (no hard-coding for easier editing)
      Query4.BtnBack.Left := BtnBack.Left;
      Query4.BtnBack.Top := BtnBack.Top;
      Query4.BtnNext.Left := BtnNext.Left;
      Query4.BtnNext.Top := BtnNext.Top;
    end
    else
      // in custom setup go on to TQuery
    begin
      showForm(Query, self);
      Query.BtnBack.Left := BtnBack.Left;
      Query.BtnBack.Top := BtnBack.Top;
      Query.BtnNext.Left := BtnNext.Left;
      Query.BtnNext.Top := BtnNext.Top;
    end;
  end;
end;

procedure TQuickInstall.ComboBoxLanguagesChange(Sender: TObject);
begin
  if ComboBoxLanguages.Text = 'Deutsch' then
  begin
    SetDefaultLang('de');
    SetBtnWidth('de');
    // Somehow the following made problems with de->en->de translation so we set
    // it here always again.
    LabelCarryOut.Caption := rsCarryOut;
  end
  else if ComboBoxLanguages.Text = 'English' then
  begin
    SetDefaultLang('en');
    SetBtnWidth('en');
  end
  else if ComboBoxLanguages.Text = 'Español' then
  begin
    SetDefaultLang('es');
    SetBtnWidth('es');
    LabelCarryOut.Caption := rsCarryOut;
  end
  else if ComboBoxLanguages.Text = 'Français' then
  begin
    SetDefaultLang('fr');
    SetBtnWidth('fr');
    LabelCarryOut.Caption := rsCarryOut;
  end;
end;

procedure TQuickInstall.FormActivate(Sender: TObject);
begin
  inherited FormActivate(Sender);
  if not isValidFQDN(GetFQDNUnix) then
    ShowMessage(rsInvalidFqdnWarning);
end;

end.
