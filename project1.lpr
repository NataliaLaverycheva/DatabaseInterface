program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, Connect, MetaData, References, Editing, TimeTable, Conflicts;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TSchedule, Schedule);
  Application.CreateForm(TConnectForm, ConnectForm);
  Application.CreateForm(TReferences, FReferences);
  //Application.CreateForm(TEditForm, EditForm);
  Application.CreateForm(TT, T);
  //Application.CreateForm(TConflictForm, ConflictForm);
  Application.Run;
end.

