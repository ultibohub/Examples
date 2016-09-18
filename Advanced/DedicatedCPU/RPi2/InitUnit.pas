unit InitUnit;

{$mode objfpc}{$H+}

{ Advanced example - Dedicated CPU                                             }
{                                                                              }
{ This file is used during initialization of the program and allows us to make }
{ some changes to the startup behaviour so that we prevent certain threads from}
{ being assigned to our dedicated CPU. You can try commenting this out to see  }
{ the affect on the behavior, see the comments in the ThreadUnit for more info }

interface

uses
  GlobalConst,
  Threads;

implementation
 
initialization
 {Disable thread allocation for CPU 3}
 SchedulerAllocationDisable(CPU_ID_3);
end.
