unit opsi_quick_install_unit_query4;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  OpsiLinuxInstaller_QueryForm,
  FormAppearanceFunctions,
  opsi_quick_install_resourcestrings,
  opsiquickinstall_QueryData;

type

  TQuery4 = class(TOpsiLinuxInstallerQueryForm)
    EditPasswordUCS: TEdit;
    InfoReboot: TImage;
    InfoDhcp: TImage;
    InfoTFTPROOT: TImage;
    LabelDhcp: TLabel;
    LabelFilePointer: TLabel;
    LabelPasswordMasterAdmin: TLabel;
    LabelReboot: TLabel;
    PanelPasswordMasterAdmin: TPanel;
    PanelRadiofilePointer: TPanel;
    PanelRadioDhcp: TPanel;
    PanelDhcp: TPanel;
    PanelFilePointer: TPanel;
    PanelRadioReboot: TPanel;
    PanelReboot: TPanel;
    RadioBtnDhcpNo: TRadioButton;
    RadioBtnDhcpYes: TRadioButton;
    RadioBtnMenu: TRadioButton;
    RadioBtnNo: TRadioButton;
    RadioBtnNoMenu: TRadioButton;
    RadioBtnYes: TRadioButton;
    procedure BtnBackClick(Sender: TObject); override;
    procedure BtnNextClick(Sender: TObject); override;
    procedure FormActivate(Sender: TObject); override;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction); override;
    procedure RadioBtnDhcpYesChange(Sender: TObject);
  end;

var
  Query4: TQuery4;

implementation

uses
  opsi_quick_install_unit_language,
  opsi_quick_install_unit_query2,
  opsi_quick_install_unit_query5_dhcp,
  opsi_quick_install_unit_query6;

{$R *.lfm}

procedure TQuery4.BtnNextClick(Sender: TObject);
begin
  // Make Data entries
  // UCS password
  Data.ucsPassword := EditPasswordUCS.Text;
  // Reboot
  if RadioBtnYes.Checked then
    Data.reboot.SetEntries(RadioBtnYes.Caption, 'true')
  else
    Data.reboot.SetEntries(RadioBtnNo.Caption, 'false');
  // Dhcp
  if RadioBtnDhcpYes.Checked then
    Data.dhcp.SetEntries(RadioBtnDhcpYes.Caption, 'true')
  else
    Data.dhcp.SetEntries(RadioBtnDhcpNo.Caption, 'false');
  // TFTPROOT
  if RadioBtnMenu.Checked then
    Data.symlink := RadioBtnMenu.Caption
  else
    Data.symlink := RadioBtnNoMenu.Caption;

  // if a dhcp-server is wanted, show an own form for the required network information
  if RadioBtnDhcpYes.Checked then
  begin
    showForm(Query5_dhcp, self);
    Query5_dhcp.BtnBack.Left := BtnBack.Left;
    Query5_dhcp.BtnBack.Top := BtnBack.Top;
    Query5_dhcp.BtnNext.Left := BtnNext.Left;
    Query5_dhcp.BtnNext.Top := BtnNext.Top;
  end
  else
  begin
    showForm(Query6, self);
    Query6.BtnBack.Left := BtnBack.Left;
    Query6.BtnBack.Top := BtnBack.Top;
    Query6.BtnNext.Left := BtnNext.Left;
    Query6.BtnNext.Top := BtnNext.Top;
  end;
end;

procedure TQuery4.FormActivate(Sender: TObject);
begin
  inherited FormActivate(Sender);

  PanelFilePointer.AutoSize := False;
  PanelFilePointer.AutoSize := True;
  // ask for UCS password only if distribution is Univention
  if lowerCase(Data.DistrInfo.DistroName) = 'univention' then
    PanelPasswordMasterAdmin.Visible := True
  else
  begin
    PanelPasswordMasterAdmin.Visible := False;
  end;

  // ask for reboot only in custom setup
  if not Data.CustomSetup then
  begin
    PanelReboot.Visible := False;
    InfoReboot.Visible := False;
  end
  else
  begin
    PanelReboot.Visible := True;
    InfoReboot.Visible := True;
  end;

  // text by resourcestrings
  // adjust form caption depending on setup type
  if not Data.CustomSetup then
    Caption := 'Opsi Quick Install - ' + rsCapQuery
  else
    Caption := 'Opsi Quick Install - ' + rsCapQuery3;
  LabelPasswordMasterAdmin.Caption := rsUCS;
  LabelReboot.Caption := rsReboot;
  InfoReboot.Hint := rsInfoReboot;
  RadioBtnYes.Caption := rsYes;
  RadioBtnNo.Caption := rsNo;
  LabelDhcp.Caption := rsDhcp;
  InfoDhcp.Hint := rsInfoDhcp;
  RadioBtnDhcpYes.Caption := rsYes;
  RadioBtnDhcpNo.Caption := rsNo;
  LabelFilePointer.Caption := rsTFTPROOT;
  InfoTFTPROOT.Hint := rsInfoTFTPROOT;
  BtnBack.Caption := rsBack;
  BtnNext.Caption := rsNext;
end;

procedure TQuery4.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  Query2.Close;
end;

procedure TQuery4.RadioBtnDhcpYesChange(Sender: TObject);
begin
  // ask for symlink only if a dhcp-server is wanted
  if RadioBtnDhcpYes.Checked then
  begin
    PanelFilePointer.Visible := True;
    InfoTFTPROOT.Visible := True;
  end
  else
  begin
    PanelFilePointer.Visible := False;
    InfoTFTPROOT.Visible := False;
  end;
end;

procedure TQuery4.BtnBackClick(Sender: TObject);
begin
  if not Data.CustomSetup then
    showForm(QuickInstall, self)
  else
  begin
    showForm(Query2, self);
  end;
end;

end.
