unit Connect;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, IBConnection, FileUtil, Forms, Controls, Graphics,
  Dialogs;

type

  { TConnectForm }

  TConnectForm = class(TForm)
    IBConnection: TIBConnection;
    SQLTransaction: TSQLTransaction;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ConnectForm: TConnectForm;

implementation

{$R *.lfm}

{ TConnectForm }

procedure TConnectForm.FormCreate(Sender: TObject);
begin
end;

end.

