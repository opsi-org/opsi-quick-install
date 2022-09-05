unit opsi_quick_install_unit_query6;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  OpsiLinuxInstaller_QueryForm,
  FormAppearanceFunctions;

type

  TQuery6 = class(TOpsiLinuxInstallerQueryForm)
    EditNameAdmin: TEdit;
    EditNameIP: TEdit;
    EditNumberIP: TEdit;
    EditPasswordAdmin: TEdit;
    InfoAdmin: TImage;
    LabelNameAdmin: TLabel;
    LabelNameIP: TLabel;
    LabelNumberIP: TLabel;
    LabelPasswordAdmin: TLabel;
    PanelNameAdmin: TPanel;
    PanelNameIP: TPanel;
    PanelNumberIP: TPanel;
    PanelPasswordAdmin: TPanel;
    procedure BtnBackClick(Sender: TObject); override;
    procedure BtnNextClick(Sender: TObject); override;
    procedure EditNameAdminChange(Sender: TObject);
    procedure FormActivate(Sender: TObject); override;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction); override;
  end;

var
  Query6: TQuery6;

implementation

uses
  opsi_quick_install_resourcestrings,
  opsiquickinstall_data,
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query4,
  opsi_quick_install_unit_query5_dhcp,
  opsi_quick_install_unit_overview;

{$R *.lfm}

procedure TQuery6.BtnNextClick(Sender: TObject);
begin
  // Make Data Entries
  Data.adminName := EditNameAdmin.Text;
  Data.adminPassword := EditPasswordAdmin.Text;
  Data.ipName := EditNameIP.Text;
  Data.ipNumber := EditNumberIP.Text;

  showForm(Overview, self);
  Overview.BtnBack.Left := BtnBack.Left;
  Overview.BtnBack.Top := BtnBack.Top;
  Overview.BtnFinish.Left := Overview.Width - Overview.BtnBack.Left -
    QuickInstall.BtnFinishWidth;
  Overview.BtnFinish.Top := BtnNext.Top;
end;

procedure TQuery6.EditNameAdminChange(Sender: TObject);
begin
  // ask for admin password only if a new admin is wanted
  if EditNameAdmin.Text = '' then
    PanelPasswordAdmin.Visible := False
  else
    PanelPasswordAdmin.Visible := True;
end;

procedure TQuery6.FormActivate(Sender: TObject);
begin
  inherited FormActivate(Sender);

  PanelPasswordAdmin.AutoSize := False;
  PanelPasswordAdmin.AutoSize := True;
  // text by resourcestrings
  Caption := 'Opsi Quick Install - ' + rsCapQueryUserInfo;
  LabelNameAdmin.Caption := rsAdminName;
  InfoAdmin.Hint := rsInfoAdmin;
  LabelPasswordAdmin.Caption := rsAdminPassword;
  LabelNameIP.Caption := rsIPName;
  LabelNumberIP.Caption := rsIPNumber;
  BtnBack.Caption := rsBack;
  BtnNext.Caption := rsNext;
end;

procedure TQuery6.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  Query5_dhcp.Close;
end;

procedure TQuery6.BtnBackClick(Sender: TObject);
begin
  if Query4.RadioBtnDhcpYes.Checked then
    showForm(Query5_dhcp, self)
  else
    showForm(Query4, self);
end;

end.
