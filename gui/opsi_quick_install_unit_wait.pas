unit opsi_quick_install_unit_wait;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  opsi_quick_install_resourcestrings;

type

  TWait = class(TForm)
    LabelWait: TLabel;
    procedure FormActivate(Sender: TObject);
  end;

var
  Wait: TWait;

implementation

uses
  opsi_quick_install_unit_overview;

{$R *.lfm}

procedure TWait.FormActivate(Sender: TObject);
begin
  Left := Overview.Left + Round(Overview.Width / 2) - Round(Width / 2);
  Top := Overview.Top + Round(Overview.Height / 2) - Round(Height / 2);
  // text by resourcestrings
  LabelWait.Caption := rsWait + #10 + ' ';
end;

end.

