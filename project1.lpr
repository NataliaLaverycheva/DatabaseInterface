program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, Connect, MetaData, References, Editing, TimeTable;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TSchedule, Schedule);
  Application.CreateForm(TConnectForm, ConnectForm);
  Application.CreateForm(TReferences, FReferences);
  //Application.CreateForm(TEditForm, EditForms);
  Application.CreateForm(TT, T);
  Application.Run;
end.

