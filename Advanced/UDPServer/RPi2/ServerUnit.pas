unit ServerUnit;

{$mode objfpc}{$H+}

{ Advanced example - UDP Server                                                }
{                                                                              }
{ This file contains the main functionality for our UDP server example.        }
{                                                                              }
{ Here we create a class that defines the behaviour of our server and a couple }
{ of functions to initialize and start our server example.                     }

interface

uses
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Console,   {Include the console unit so we can output logging to the screen}
  Winsock2;  {Include the Winsock2 unit to provide access to the TWinsock2UDPListener class}

  
{There are primarily two ways to use the TWinsock2UDPListener class to create a UDP server.

 The first way is to simply create an instance of the class and register an OnExecute handler
 method to control what happens whenever a request is received.
 
 The second way, which we are doing for this example, is to create a class that descends from
 TWinsock2UDPListener and override the DoExecute method to define what will happen with each
 request received.
 
 Both methods work equally well, in many cases you may want to define a descendant class anyway
 in order to add extra functionality and customized behavior}
  
type
 {Create a TDemoUDPListener which descends from TWinsock2UDPListener}
 TDemoUDPListener = class(TWinsock2UDPListener)
  constructor Create;
  destructor Destroy; override;
 private
  {Add a window handle so we can write to our console window}
  FWindowHandle:TWindowHandle;
   
 protected
  {Override the DoExecute method to control request handling}
  function DoExecute(AThread:TWinsock2UDPServerThread):Boolean; override;
 end; 
  
var
 {A variable to store an instance of our class}
 DemoUDPListener:TDemoUDPListener;

 
{A couple of simple functions to initialize and start our server}
procedure ServerInit;
procedure ServerStart;
  
implementation


{The overridden Create method for our class, here all we want to do is create a 
 console window and store the window handle. We also output a simple ready message}
constructor TDemoUDPListener.Create;
begin
 {Call the inherited Create}
 inherited Create;
 
 {Create a console window}
 FWindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,False);
 
 {Output a message}
 ConsoleWindowWriteLn(FWindowHandle,'Demo UDP Server ready');
end;


{The destructor for our class, simply destroy the console window before calling the
 inherited destroy method}
destructor TDemoUDPListener.Destroy; 
begin
 {Destroy our console window}
 ConsoleWindowDestroy(FWindowHandle);
 
 {Call the inherited Destroy}
 inherited Destroy;
end;


{The main part of our UDP server example, the overridden DoExecute method will be
 called everytime a request (or message) is received by our server.
 
 The important thing to understand here is that this method will be called by one
 of the UDP Server threads that are created for the thread pool. 
 
 As each request is received the UDP Listener thread will obtain the next available
 UDP Server thread from the pool and allocate the request to it. At that point the
 UDP Listener thread will go back to waiting for more requests.
 
 Why is this important? Because using this model of a listener thread and a pool
 of worker threads allows many requests to be serviced simultaneously. It is also
 important because multiple threads may be calling this function at the same time
 so any global variables used should be protected with locks where necessary}
function TDemoUDPListener.DoExecute(AThread:TWinsock2UDPServerThread):Boolean;
var
 MessageText:String;
begin
 {Call the inherited DoExecute, which will also call the OnExecute handler if one is registered}
 Result:=inherited DoExecute(AThread);
 if not Result then Exit;

 {This function receives only one parameter which is a Thread object (TWinsock2UDPServerThread).
  From this we can access everything we need to know about the request and where it came from.
  
  The Thread object contains a Server object, if either one is invalid then we should not proceed}
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;
 
 {We can learn the IP address and port of whoever sent us this request using the Server.PeerAddress and PeerPort properties.
  
  The Server object also gives us the request data, the size of the request is found in the Server.Count property}
 ConsoleWindowWriteLn(FWindowHandle,'Received SysLog message from: ' + AThread.Server.PeerAddress + ':' + IntToStr(AThread.Server.PeerPort));
 ConsoleWindowWriteLn(FWindowHandle,'                Message count: ' + IntToStr(AThread.Server.Count));

 {If the count is greater than 0 (which it always should be) then we can extract the data and process it}
 if AThread.Server.Count > 0 then
  begin
   {For our example we want the data as a string so we can print it to the console window. In many cases the data will
    already be in some predefined structure based on the protocol you are implementing.
    
    Copy the data from the Server.Data property}
   SetLength(MessageText,AThread.Server.Count);
   Move(AThread.Server.Data^,PChar(MessageText)^,AThread.Server.Count);
   
   {Now we have the SysLog data as a string we can simply write it to the console}
   ConsoleWindowWriteLn(FWindowHandle,'                Message text: ' + MessageText);
   
   {The TWinsock2UDPListener class also allows sending data as well as receiving. Since we are running here
    as one of the pooled server threads we can happily take any amount of time we need to process the request.
    If more requests arrive then the listener thread will simply create more server threads to handle them, up
    to the maximum numbers we have defined.
    
    So for our SysLog server example we could actually forward the received log message to another external
    server by calling the SendDataTo function and passing the address and port to send to.
    
    Try it out yourself by setting the address to something valid}
   //SendDataTo('192.168.123.123',514,PChar(MessageText),Length(MessageText)); 
  end;
end;


{Perform some simple initialization of our server, mainly we want to wait until
 the network is up and an IP address has been assigned to avoid any complications
 with trying to use the Winsock functions before the network is ready}
procedure ServerInit;
var
 IPAddress:String;
 Winsock2TCPClient:TWinsock2TCPClient;
begin
 {Create a Winsock2TCPClient so that we can get some local information}
 Winsock2TCPClient:=TWinsock2TCPClient.Create;

 {Get our local IP address which may be invalid at this point}
 IPAddress:=Winsock2TCPClient.LocalAddress;

 {Check the local IP address}
 if (IPAddress = '') or (IPAddress = '0.0.0.0') or (IPAddress = '255.255.255.255') then
  begin
   {Wait until we have an IP address}
   while (IPAddress = '') or (IPAddress = '0.0.0.0') or (IPAddress = '255.255.255.255') do
    begin
     {Sleep a bit}
     Sleep(1000);

     {Get the address again}
     IPAddress:=Winsock2TCPClient.LocalAddress;
    end;
  end;
 
 {Free the Winsock2TCPClient object}
 Winsock2TCPClient.Free;
end;


{Here we create the instance of our UDP listener class and set some parameters
 to customize the way it operates.}
procedure ServerStart;
var
 WSAData:TWSAData;
begin
 {Perform the normal Winsock startup process}
 FillChar(WSAData,SizeOf(TWSAData),0);
 if WSAStartup(WINSOCK_VERSION,WSAData) = ERROR_SUCCESS then
  begin
   {Create our TDemoUDPListener object}
   DemoUDPListener:=TDemoUDPListener.Create;
  
   {Set the minimum and maximum number of threads to service requests. The TWinsock2UDPListener
    has a pool of threads which can be dynamically expanded to accomodate extra requests and will
    also shrink when no requests are happening. The Min and Max values determine the number of
    threads for each case}
   DemoUDPListener.Threads.Min:=5;
   DemoUDPListener.Threads.Max:=10;
  
   {Set the buffer size to 1024 (The maximum for UDP SysLog). The TWinsock2UDPListener also
    has a dynamic buffer pool which contains preallocated buffers based on a size you specify.
    
    Since UDP is a connectionless protocol, all communication occurs as messages or datagrams.
    Many common services that use UDP will have a fixed length or well defined message size so
    buffers can be allocated that suit the required size}
   DemoUDPListener.BufferSize:=1024;
   
   {Set the port to listen on (514 for SysLog)}
   DemoUDPListener.BoundPort:=514;
   
   {Set the server to active (Listener)} 
   DemoUDPListener.Active:=True;
   
   {At this point our UDP server has been started independently of our current thread and will
    continue to run by itself even if this thread terminates. For the sake of the example we will
    go into a loop and send logging messages which should be received by our server}
   while True do
    begin
     Sleep(1000);
     LoggingOutput('Logging message sent by ' + ThreadGetName(ThreadGetCurrent) + ' at ' + DateTimeToStr(Now));
    end;
    
   {Destroy the UDP Listener}
   DemoUDPListener.Free;
  end; 
end;


end.

