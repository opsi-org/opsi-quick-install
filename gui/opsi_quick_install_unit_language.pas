unit opsi_quick_install_unit_language;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, process,
  osnetutil,
  osfuncunix,
  OpsiLinuxInstaller_WelcomeForm,
  FormAppearanceFunctions,
  LogFileFunctions,
  oslog,
  opsiquickinstall_QueryData,
  opsi_quick_install_CommonResourceStrings,
  opsi_quick_install_GuiResourceStrings,
  OpsiLinuxInstaller_LanguageObject;

type

  TQuickInstall = class(TOpsiLinuxInstallerWelcomeForm)
    LabelSetup: TLabel;
    QuickInstallPanel: TPanel;
    RadioBtnDefault: TRadioButton;
    RadioBtnCustom: TRadioButton;
    procedure SetDefaultLanguage(const Languages: TStringList);
    procedure FillLanguageSelection;
    procedure SetTextsByResourceStrings; override;
    procedure GetBtnFinishWidth;
    procedure BtnNextClick(Sender: TObject); override;
    procedure ComboBoxLanguagesChange(Sender: TObject);
    procedure FormActivate(Sender: TObject); override;
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
  opsi_quick_install_unit_distr,
  opsi_quick_install_unit_query,
  opsi_quick_install_unit_query4;

{$R *.lfm}

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
  Language.Abbreviation := Copy(GetEnvironmentVariable('LANG'), 1, 2);
  // let the combo box show the system language at the beginning
  ComboBoxLanguages.ItemIndex := Languages.IndexOf(Language.Abbreviation);
  // now set position of BtnNext for the default language
  SetBtnWidth(Language.Abbreviation);
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

procedure TQuickInstall.SetTextsByResourceStrings;
begin
  inherited SetTextsByResourceStrings;

  Language.TranslateResourceStrings('opsi_quick_install_CommonResourceStrings',
    'opsi_quick_install_CommonResourceStrings.' + Language.Abbreviation + '.po');
  Language.TranslateResourceStrings('opsi_quick_install_GuiResourceStrings',
    'opsi_quick_install_GuiResourceStrings.' + Language.Abbreviation + '.po');

  LabelSetup.Caption := rsSetup;
  RadioBtnDefault.Caption := rsStandard;
  RadioBtnCustom.Caption := rsCustom;
end;

procedure TQuickInstall.FormCreate(Sender: TObject);
begin
  inherited FormCreate(Sender);
  CenterFormOnScreen(Sender as TForm);

  Language := TLanguageObject.Create(
    '../../../lazarus/common/OpsiLinuxInstaller/locale/',
    '../locale/');

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

  SetTextsByResourceStrings;
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
  case ComboBoxLanguages.Text of
    'Deutsch': Language.Abbreviation := 'de';
    'English': Language.Abbreviation := 'en';
    'Español': Language.Abbreviation := 'es';
    'Français': Language.Abbreviation := 'fr';
  end;
  SetBtnWidth(Language.Abbreviation);
  SetTextsByResourceStrings;
end;

procedure TQuickInstall.FormActivate(Sender: TObject);
begin
  inherited FormActivate(Sender);
  if not isValidFQDN(GetFQDNUnix) then
    ShowMessage(rsInvalidFqdnWarning);
end;

end.
