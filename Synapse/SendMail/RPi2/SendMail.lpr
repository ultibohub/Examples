program SendMail;

{$mode delphi}{$H+}

{ Synapse example - Send Mail                                                  }
{                                                                              }
{ A example of using Synapse to send an email message.                         }
{                                                                              }
{ Change the values of ToAddress and FromAddress below to set the email source }
{ and destination addresses.                                                   }
{                                                                              }
{ To compile the example select Run, Compile (or Run, Build) from the menu.    }
{                                                                              }
{ Once compiled copy the kernel7.img file to an SD card along with the         }
{ firmware files and use it to boot your Raspberry Pi.                         }
{                                                                              }
{ This version is for Raspberry Pi 2B and will also work on a 3B.              }
{                                                                              }
{ This example was adapted for Ultibo from the Synapse MailCheck example.      }
{                                                                              }
{ Note: If you downloaded this example separate from the Synapse zip file      }
{ then you may need to add the path to the Synapse sources to your project     }
{ options.                                                                     }
{                                                                              }
{ To do that go to Project, Project Options... in the Lazarus menu and then    }
{ select Compiler Options, Other unit files (-Fu) and enter the correct path.  }
{                                                                              }

uses
  RaspberryPi2,
  GlobalConst,
  Threads,
  Classes,
  SysUtils,
  Console,
  mailsend;

var
 Handle:THandle;
  
 ToAddress:String = 'someone@somewhere.com'; {Change these addresses to use your own}
 FromAddress:String = 'someone@somewhere.com';

begin
 {Create our console window}
 Handle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);
  
 ConsoleWindowWriteLn(Handle,'Starting Synapse Send Mail Example');
  
 {Wait a couple of seconds for C:\ drive to be ready}
 ConsoleWindowWriteLn(Handle,'Waiting for drive C:\');
 while not DirectoryExists('C:\') do
  begin
   {Sleep for a second}
   Sleep(1000);
  end;
 ConsoleWindowWriteLn(Handle,'C:\ drive is ready');
 ConsoleWindowWriteLn(Handle,'');
 
 {Wait a few seconds for the network to be ready}
 Sleep(3000);
 
 {Send the email}
 {If you receive "Login failed" messages it may be because your ISP blocks outbound 
  connections to port 25, this ia common industry practive to prevent spam. You may
  need to set the EmailHost variable in the MailSend unit to the mail host of your ISP}
 if SendEmail(ToAddress,FromAddress) then
  begin
   ConsoleWindowWriteLn(Handle,'Email sent successfully');
  end
 else
  begin
   ConsoleWindowWriteLn(Handle,'Send email failed');
  end;  
 
 {Halt the main thread} 
 ThreadHalt(0);
end.
 