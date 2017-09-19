unit GLESUnit;

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{This example is directly ported from the hello triangle example in the hello_pi
 collection included with Raspbian. The original source can be found here:
 
 https://github.com/raspberrypi/userland/blob/master/host_applications/linux/apps/hello_pi/hello_triangle
 
 and if you compare it against this you will notice that most things are very similar.
 
 This means that you should be able to port many OpenGL ES examples to Ultibo with
 only minimal changes to account for the different environment.
 
 It is not the purpose of this example to teach you how to use OpenGL ES, for that
 you should consult one of the many online references and tutorials which cover
 everything from the very basic fundamentals to highly advanced topics.
 
 You can find the official Khronos reference for OpenGL ES 1.1 here:
 
 https://www.khronos.org/registry/OpenGL-Refpages/es1.1/xhtml/

} 

interface

{The VideoCore IV GPU supports both OpenGL ES 1.1 and 2.0 so we have enabled the Free Pascal units
 for both versions in Ultibo. When using OpenGL ES 1.1 and including the GLES11 unit you will also
 need to include EGL in order to access the functions required to setup and initialize OpenGL ES}
uses GlobalConst,GlobalConfig,Console,Classes,SysUtils,EGL,GLES11,DispmanX,VC4,Math,uBitmap;
 
const
 {The value of Pi, available in many places but defined here just to be sure}
 M_PI = 3.141592654;
 
const
 {The spatial coordinates for the 3D cube (6 sides with 4 vertices per side and 3 values per vertex)}
 QuadX:array[0..(6 * 4 * 3) - 1] of GLbyte = (
  {FRONT}
  -10, -10,  10,
  10, -10,  10,
  -10,  10,  10,
  10,  10,  10,

  {BACK}
  -10, -10, -10,
  -10,  10, -10,
  10, -10, -10,
  10,  10, -10,

  {LEFT}
  -10, -10,  10,
  -10,  10,  10,
  -10, -10, -10,
  -10,  10, -10,

  {RIGHT}
  10, -10, -10,
  10,  10, -10,
  10, -10,  10,
  10,  10,  10,

  {TOP}
  -10,  10,  10,
  10,  10,  10,
  -10,  10, -10,
  10,  10, -10,

  {BOTTOM}
  -10, -10,  10,
  -10, -10, -10,
  10, -10,  10,
  10, -10, -10
 );

 {The texture coordinates for the cube}
 TexCoords:array[0..(6 * 4 * 2) - 1] of GLfloat = (
  0.0,  0.0,
  0.0,  1.0,
  1.0,  0.0,
  1.0,  1.0,
  
  0.0,  0.0,
  0.0,  1.0,
  1.0,  0.0,
  1.0,  1.0,
  
  0.0,  0.0,
  0.0,  1.0,
  1.0,  0.0,
  1.0,  1.0,
  
  0.0,  0.0,
  0.0,  1.0,
  1.0,  0.0,
  1.0,  1.0,
  
  0.0,  0.0,
  0.0,  1.0,
  1.0,  0.0,
  1.0,  1.0,
  
  0.0,  0.0,
  0.0,  1.0,
  1.0,  0.0,
  1.0,  1.0
 );
 
type
 {A structure to keep track of the overall state including the DispmanX and EGL handles}
 {GLES State}
 PGLESState = ^TGLESState;
 TGLESState = record
  {Screen dimensions}
  ScreenWidth:LongWord;
  ScreenHeight:LongWord;
  {DispmanX window}
  DispmanDisplay:DISPMANX_DISPLAY_HANDLE_T;
  DispmanElement:DISPMANX_ELEMENT_HANDLE_T;
  {EGL data}
  Display:EGLDisplay;
  Surface:EGLSurface;
  Context:EGLContext;
  {EGL config}
  NativeWindow:EGL_DISPMANX_WINDOW_T;
  AttributeList:array[0..10] of EGLint;
 end;
 
 {Another structure to track the details of our OpenGL ES scene}
 {GLES Scene}
 PGLESScene = ^TGLESScene;
 TGLESScene = record
  {Textures}
  Textures:array[0..5] of GLuint;
  {Model rotation vector and direction}
  RotationAngleXIncrement:GLfloat;
  RotationAngleYIncrement:GLfloat;
  RotationAngleZIncrement:GLfloat;
  {Current model rotation angles}
  RotationAngleX:GLfloat;
  RotationAngleY:GLfloat;
  RotationAngleZ:GLfloat;
  {Current distance from camera}
  Distance:GLfloat;
  DistanceIncrement:GLfloat;
  {Pointer to texture buffer}
  TextureBuffer:PGLvoid;
  {Image properties}
  ImageWidth:Integer;
  ImageHeight:Integer;
  ImageFormat:Integer;
 end;
 
{The main functions of our GLES example} 
procedure StartGLES;
procedure CleanupGLES(var State:TGLESState;var Scene:TGLESScene);

{Functions dealing with initializing, updating and rendering our OpenGL ES scene}
function GLESInit(var State:TGLESState):Boolean;

procedure GLESInitModelProjection(var State:TGLESState;var Scene:TGLESScene);
procedure GLESInitTextures(var Scene:TGLESScene);

procedure GLESResetModel(var Scene:TGLESScene);
procedure GLESUpdateModel(var Scene:TGLESScene);
procedure GLESRedrawScene(var State:TGLESState;var Scene:TGLESScene);

{Some utility functions to handle loading images and updating positions}
function GLESLoadTextureImages(var Scene:TGLESScene):Boolean;

function GLESIncrementAndWrapAngle(Angle,Increment:GLfloat):GLfloat;
function GLESIncrementAndClipDistance(Distance,Increment:GLfloat):GLfloat;

implementation

procedure StartGLES;
var
 State:TGLESState;
 Scene:TGLESScene;
begin
 {}
 {This is the stating point for our OpenGL ES example, this routine sets up all of the
  required elements and then continually redraws our scene onto the screen until a key
  is pressed}
  
 {All applications using the VideoCore IV must call BCMHostInit before doing any other
  operations. This will initialize all of the supporting libraries and start the VCHIQ
  communication service. Applications should also call BCMHostDeinit when they no longer
  require any VC4 services}
 BCMHostInit;
 
 {Clear our state and scene}
 FillChar(State,SizeOf(TGLESState),0);
 FillChar(Scene,SizeOf(TGLESScene),0);
 
 {Initialize the OpenGL ES state}
 GLESInit(State);
 
 {Setup the model world}
 GLESInitModelProjection(State,Scene);
 
 {Initialize the OpenGLES textures}
 GLESInitTextures(Scene);
 
 {Empty the keyboard buffer before starting our loop}
 while ConsoleKeypressed do
  begin
   ConsoleReadKey;
  end;
  
 while True do
  begin
   {Update our model and apply the next rotation}
   GLESUpdateModel(Scene);
   
   {Render the scene and display again}
   GLESRedrawScene(State,Scene);
   
   {Check for a keypress and break if found}
   if ConsoleKeyPressed then Break;
  end;
  
 {Cleanup the OpenGL ES state and scene} 
 CleanupGLES(State,Scene);
 
 {Deinitialize the VC4}
 BCMHostDeinit;
end;

procedure CleanupGLES(var State:TGLESState;var Scene:TGLESScene);
var
 Success:Integer;
 DispmanUpdate:DISPMANX_UPDATE_HANDLE_T;
begin
 {}  
 {Clear the screen}
 glClear(GL_COLOR_BUFFER_BIT);
 eglSwapBuffers(State.Display,State.Surface);

 {Delete the OpenGL ES textures}
 glDeleteTextures(6,@Scene.Textures);
 
 {Destroy the EGL surface}
 eglDestroySurface(State.Display,State.Surface);
 
 {Begin a DispmanX update}
 DispmanUpdate:=vc_dispmanx_update_start(0);
 
 {Remove the DispmanX element}
 Success:=vc_dispmanx_element_remove(DispmanUpdate,State.DispmanElement);
 if Success <> 0 then Exit; 

 {And submit the change to DispmanX}
 vc_dispmanx_update_submit_sync(DispmanUpdate);
 
 {Close the DispmanX display we opened earlier}
 Success:=vc_dispmanx_display_close(State.DispmanDisplay);
 if Success <> 0 then Exit; 

 {Release OpenGL resources and terminate EGL}
 eglMakeCurrent(State.Display,EGL_NO_SURFACE,EGL_NO_SURFACE,EGL_NO_CONTEXT);
 eglDestroyContext(State.Display,State.Context);
 eglTerminate(State.Display);

 {Release our texture buffer}
 FreeMem(Scene.TextureBuffer);
end;

function GLESInit(var State:TGLESState):Boolean;
var
 Config:EGLConfig;
 ConfigCount:EGLint;
 EGLResult:EGLBoolean;

 DestRect:VC_RECT_T;
 SourceRect:VC_RECT_T;
 DispmanUpdate:DISPMANX_UPDATE_HANDLE_T;
begin
 {}
 {This function provides a good general outline of how to initialize OpenGL ES 1.1 on the
  VideoCore IV and obtain the necessary handles and contexts.
  
  You will need something similar to this in all OpenGL ES applications before you can
  perform any OpenGL ES operations. 
  
  For an OpenGL ES 2.0 version of this function please see the HelloGLES2 example}
 Result:=False;
 
 {Setup some DispmanX and EGL defaults}
 State.DispmanDisplay:=DISPMANX_NO_HANDLE;
 State.DispmanElement:=DISPMANX_NO_HANDLE;
 DispmanUpdate:=DISPMANX_NO_HANDLE;
 State.Display:=EGL_NO_DISPLAY;
 State.Surface:=EGL_NO_SURFACE;
 State.Context:=EGL_NO_CONTEXT;
 
 {Setup the EGL attribute list}
 State.AttributeList[0]:=EGL_RED_SIZE;
 State.AttributeList[1]:=8;
 State.AttributeList[2]:=EGL_GREEN_SIZE;
 State.AttributeList[3]:=8;
 State.AttributeList[4]:=EGL_BLUE_SIZE;
 State.AttributeList[5]:=8;
 State.AttributeList[6]:=EGL_ALPHA_SIZE;
 State.AttributeList[7]:=8;
 State.AttributeList[8]:=EGL_SURFACE_TYPE;
 State.AttributeList[9]:=EGL_WINDOW_BIT;
 State.AttributeList[10]:=EGL_NONE;
 
 try
  {Get an EGL display connection}
  State.Display:=eglGetDisplay(EGL_DEFAULT_DISPLAY);
  if State.Display = EGL_NO_DISPLAY then Exit;
  
  {Initialize the EGL display connection}
  EGLResult:=eglInitialize(State.Display,nil,nil);
  if EGLResult = EGL_FALSE then Exit;
  
  {Get an appropriate EGL framebuffer configuration}
  EGLResult:=eglChooseConfig(State.Display,@State.AttributeList,@Config,1,@ConfigCount);
  if EGLResult = EGL_FALSE then Exit;
  
  {Create an EGL rendering context}
  State.Context:=eglCreateContext(State.Display,Config,EGL_NO_CONTEXT,nil);
  if State.Context = EGL_NO_CONTEXT then Exit;
  
  {Create an EGL window surface}
  if BCMHostGraphicsGetDisplaySize(DISPMANX_ID_MAIN_LCD,State.ScreenWidth,State.ScreenHeight) < 0 then Exit;
  
  {Setup the DispmanX source and destination rectangles}
  vc_dispmanx_rect_set(@DestRect,0,0,State.ScreenWidth,State.ScreenHeight);
  vc_dispmanx_rect_set(@SourceRect,0 shl 16,0 shl 16,State.ScreenWidth shl 16,State.ScreenHeight shl 16);
  
  {Open the DispmanX display}
  State.DispmanDisplay:=vc_dispmanx_display_open(DISPMANX_ID_MAIN_LCD);
  if State.DispmanDisplay = DISPMANX_NO_HANDLE then Exit;
  
  {Start a DispmanX update}
  DispmanUpdate:=vc_dispmanx_update_start(0);
  if DispmanUpdate = DISPMANX_NO_HANDLE then Exit;
  
  {Add a DispmanX element for our display}
  State.DispmanElement:=vc_dispmanx_element_add(DispmanUpdate,State.DispmanDisplay,0 {Layer},@DestRect,0 {Source},@SourceRect,DISPMANX_PROTECTION_NONE,nil {Alpha},nil {Clamp},DISPMANX_NO_ROTATE {Transform});
  if State.DispmanElement = DISPMANX_NO_HANDLE then Exit;
  
  {Define an EGL DispmanX native window structure}
  State.NativeWindow.Element:=State.DispmanElement;
  State.NativeWindow.Width:=State.ScreenWidth;
  State.NativeWindow.Height:=State.ScreenHeight;
  
  {Submit the DispmanX update}
  vc_dispmanx_update_submit_sync(DispmanUpdate);
 
  {Create an EGL window surface}
  State.Surface:=eglCreateWindowSurface(State.Display,Config,@State.NativeWindow,nil);
  if State.Surface = EGL_NO_SURFACE then Exit;

  {Connect the EGL context to the EGL surface}
  EGLResult:=eglMakeCurrent(State.Display,State.Surface,State.Surface,State.Context);
  if EGLResult = EGL_FALSE then Exit;
      
  {The following items are specific to this example and may or may not apply to other applications}    
  {Set the background color and clear the buffers (Notice that alpha value is 0.0 which gives us a transparent background)}
  glClearColor(0.15,0.25,0.35,0.0);

  {Enable back face culling}
  glEnable(GL_CULL_FACE);
  
  {Set the matrix mode}
  glMatrixMode(GL_MODELVIEW);
  
  Result:=True;
 finally
  {Check if the initialization was successful, if not cleanup}
  if not Result then
   begin
    {Close the DispmanX display if opened}
    if State.DispmanDisplay <> DISPMANX_NO_HANDLE then vc_dispmanx_display_close(State.DispmanDisplay);
    
    {Check for an EGL display connection}
    if State.Display <> EGL_NO_DISPLAY then
     begin
      {Terminate EGL}
      eglMakeCurrent(State.Display,EGL_NO_SURFACE,EGL_NO_SURFACE,EGL_NO_CONTEXT);
      
      {Destroy the EGL surface}
      if State.Surface <> EGL_NO_SURFACE then eglDestroySurface(State.Display,State.Surface);
      
      {Destroy the EGL context}
      if State.Context <> EGL_NO_CONTEXT then eglDestroyContext(State.Display,State.Context);

      {Terminate the EGL display}
      eglTerminate(State.Display);
     end;
   end;
 end;
end;

procedure GLESInitModelProjection(var State:TGLESState;var Scene:TGLESScene);
var
 nearp:GLfloat;
 farp:GLfloat;
 hht:GLfloat;
 hwd:GLfloat;
begin
 {}
 {This function initializes the 3D cube, for more information please see the
  OpenGL ES documentation}
 nearp:=1.0;
 farp:=500.0;
 
 glHint(GL_PERSPECTIVE_CORRECTION_HINT,GL_NICEST);

 glViewport(0,0,State.ScreenWidth,State.ScreenHeight);
      
 glMatrixMode(GL_PROJECTION);
 glLoadIdentity;

 hht:=nearp * tan(45.0 / 2.0 / 180.0 * M_PI);
 hwd:=hht * State.ScreenWidth / State.ScreenHeight;

 glFrustumf(-hwd,hwd,-hht,hht,nearp,farp);
   
 glEnableClientState(GL_VERTEX_ARRAY);
 glVertexPointer(3,GL_BYTE,0,@QuadX);

 GLESResetModel(Scene);
end;

procedure GLESInitTextures(var Scene:TGLESScene);
begin
 {}
 {Initialize the textures for our 3D cube by binding an image to each of the 6 faces}
 
 {Load the texture buffer}
 if not GLESLoadTextureImages(Scene) then Exit;
 
 {Create six OpenGL ES texture surfaces}
 glGenTextures(6,@Scene.Textures);
 
 {Setup first texture}
 glBindTexture(GL_TEXTURE_2D,Scene.Textures[0]);
 glTexImage2D(GL_TEXTURE_2D,0,Scene.ImageFormat,Scene.ImageWidth,Scene.ImageHeight,0,Scene.ImageFormat,GL_UNSIGNED_BYTE,Scene.TextureBuffer);
 glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
 glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
 
 {Setup second texture - reuse first image}
 glBindTexture(GL_TEXTURE_2D,Scene.Textures[1]);
 glTexImage2D(GL_TEXTURE_2D,0,Scene.ImageFormat,Scene.ImageWidth,Scene.ImageHeight,0,Scene.ImageFormat,GL_UNSIGNED_BYTE,Scene.TextureBuffer);
 glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
 glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
 
 {Third texture}
 glBindTexture(GL_TEXTURE_2D,Scene.Textures[2]);
 glTexImage2D(GL_TEXTURE_2D,0,Scene.ImageFormat,Scene.ImageWidth,Scene.ImageHeight,0,Scene.ImageFormat,GL_UNSIGNED_BYTE,Scene.TextureBuffer);
 glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
 glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
 
 {Fourth texture  - reuse second image}
 glBindTexture(GL_TEXTURE_2D,Scene.Textures[3]);
 glTexImage2D(GL_TEXTURE_2D,0,Scene.ImageFormat,Scene.ImageWidth,Scene.ImageHeight,0,Scene.ImageFormat,GL_UNSIGNED_BYTE,Scene.TextureBuffer);
 glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
 glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
 
 {Fifth texture}
 glBindTexture(GL_TEXTURE_2D,Scene.Textures[4]);
 glTexImage2D(GL_TEXTURE_2D,0,Scene.ImageFormat,Scene.ImageWidth,Scene.ImageHeight,0,Scene.ImageFormat,GL_UNSIGNED_BYTE,Scene.TextureBuffer);
 glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
 glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
 
 {Sixth texture  - reuse third image}
 glBindTexture(GL_TEXTURE_2D,Scene.Textures[5]);
 glTexImage2D(GL_TEXTURE_2D,0,Scene.ImageFormat,Scene.ImageWidth,Scene.ImageHeight,0,Scene.ImageFormat,GL_UNSIGNED_BYTE,Scene.TextureBuffer);
 glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST);
 glTexParameterf(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST);
 
 {Setup overall texture environment}
 glTexCoordPointer(2,GL_FLOAT,0,@TexCoords);
 glEnableClientState(GL_TEXTURE_COORD_ARRAY);
 
 glEnable(GL_TEXTURE_2D);
end;

procedure GLESResetModel(var Scene:TGLESScene);
begin
 {}
 {Reset the default position and rotation of the 3D cube}
 
 {Reset model position}
 glMatrixMode(GL_MODELVIEW);
 glLoadIdentity;
 glTranslatef(0.0,0.0,-50.0);

 {Reset model rotation}
 Scene.RotationAngleX:=45.0;
 Scene.RotationAngleY:=30.0;
 Scene.RotationAngleZ:=0.0;
 Scene.RotationAngleXIncrement:=0.5;
 Scene.RotationAngleYIncrement:=0.5;
 Scene.RotationAngleZIncrement:=0.0;
 Scene.Distance:=40.0;
 Scene.DistanceIncrement:=0.0;
end;

procedure GLESUpdateModel(var Scene:TGLESScene);
begin
 {}
 {Update the rotation and position of the cube before rerendering}
 
 {Update position}
 Scene.RotationAngleX:=GLESIncrementAndWrapAngle(Scene.RotationAngleX,Scene.RotationAngleXIncrement);
 Scene.RotationAngleY:=GLESIncrementAndWrapAngle(Scene.RotationAngleY,Scene.RotationAngleYIncrement);
 Scene.RotationAngleZ:=GLESIncrementAndWrapAngle(Scene.RotationAngleZ,Scene.RotationAngleZIncrement);
 Scene.Distance:=GLESIncrementAndClipDistance(Scene.Distance,Scene.DistanceIncrement);

 glLoadIdentity;
 
 {Move camera back to see the cube}
 glTranslatef(0.0,0.0,-Scene.Distance);

 {Rotate model to new position}
 glRotatef(Scene.RotationAngleX,1.0,0.0,0.0);
 glRotatef(Scene.RotationAngleY,0.0,1.0,0.0);
 glRotatef(Scene.RotationAngleZ,0.0,0.0,1.0);
end;

procedure GLESRedrawScene(var State:TGLESState;var Scene:TGLESScene);
begin
 {}
 {Render our 3D cube model and swap buffers in order to display it.
 
  To understand what is happening you should consult a good OpenGL ES
  tutorial and the official documentation}
  
 {Start with a clear screen}
 glClear(GL_COLOR_BUFFER_BIT);
 
 {Draw first (front) face:}
 {Bind texture surface to current vertices}
 glBindTexture(GL_TEXTURE_2D,Scene.Textures[0]);
 
 {Need to rotate textures - do this by rotating each cube face}
 glRotatef(270.0,0.0,0.0,1.0); {front face normal along z axis}
 
 {Draw first 4 vertices}
 glDrawArrays(GL_TRIANGLE_STRIP,0,4);
 
 {Same pattern for other 5 faces - rotation chosen to make image orientation 'nice'}
 glBindTexture(GL_TEXTURE_2D,Scene.Textures[1]);
 glRotatef(90.0,0.0,0.0,1.0); {Back face normal along z axis}
 glDrawArrays(GL_TRIANGLE_STRIP,4,4);
 
 glBindTexture(GL_TEXTURE_2D,Scene.Textures[2]);
 glRotatef(90.0,1.0,0.0,0.0); {Left face normal along x axis}
 glDrawArrays(GL_TRIANGLE_STRIP,8,4);
 
 glBindTexture(GL_TEXTURE_2D,Scene.Textures[3]);
 glRotatef(90.0,1.0,0.0,0.0); {Right face normal along x axis}
 glDrawArrays(GL_TRIANGLE_STRIP,12,4);
 
 glBindTexture(GL_TEXTURE_2D,Scene.Textures[4]);
 glRotatef(270.0,0.0,1.0,0.0); {Top face normal along y axis}
 glDrawArrays(GL_TRIANGLE_STRIP,16,4);
 
 glBindTexture(GL_TEXTURE_2D,Scene.Textures[5]);
 glRotatef(90.0,0.0,1.0,0.0); {Bottom face normal along y axis}
 glDrawArrays(GL_TRIANGLE_STRIP,20,4);
 
 {Swap the buffers to display the new scene}
 eglSwapBuffers(State.Display,State.Surface);
end;

function GLESLoadTextureImages(var Scene:TGLESScene):Boolean;
var
 Size:LongWord;
 Width:LongWord;
 Height:LongWord;
 Format:LongWord;
 Filename:String;
 ImageBuffer:Pointer;
 DefaultBuffer:Pointer;
begin
 {}
 {Load a bitmap from disk and convert it to a raw image that can be passed to OpenGL ES
  for use as a texture.
  
  The VC4 internally uses BGR order for colors so we need to swap the data read from our
  bitmap into the correct format}
 Result:=False;
 
 Filename:='Logo.bmp';
 
 ConsoleWriteLn('Loading image ' + Filename);
 
 {Get the size of our bitmap, we use a new helper function from the uBitmap unit which
  will tell us how big the data will be when extracted from the file}
 Size:=0;
 LoadBitmapToBuffer(Filename,Size,Size,Width,Height,Format);
 if Size = 0 then Exit;
 
 {Once we have the size allocate a buffer for loading the image}
 ImageBuffer:=GetMem(Size);
 
 {And call LoadBitmapToBuffer again to actually load it}
 if not LoadBitmapToBuffer(Filename,ImageBuffer^,Size,Width,Height,Format) then Exit;
 
 ConsoleWriteLn('Image size is ' + IntToStr(Width) + ' x ' + IntToStr(Height));
  
 {Check what format the image is in}
 if Format = COLOR_FORMAT_RGB24 then
  begin
   {If the format is RGB24 (3 bytes per pixel)}
   {Update the size to be correct for ABGR format}
   Size:=Width * Height * 4;
   
   {Allocate another buffer to store the image in default color format}
   DefaultBuffer:=GetMem(Size);
   
   {Convert the image format to COLOR_FORMAT_DEFAULT (The default in Ultibo is COLOR_FORMAT_ARGB32)}
   PixelsFormatToDefault(Format,ImageBuffer,DefaultBuffer,Width * Height,False);
   
   {Allocate our actual texture buffer for OpenGL ES}
   Scene.TextureBuffer:=GetMem(Size);
   
   {Finally convert the image from default format to COLOR_FORMAT_ABGR32 as expected by the VC4}
   PixelsDefaultToFormat(COLOR_FORMAT_ABGR32,DefaultBuffer,Scene.TextureBuffer,Width * Height,False);
   
   {Store the format for our scene}
   Scene.ImageFormat:=GL_RGBA;
   
   {Free the image buffer}
   FreeMem(ImageBuffer);
   
   {Free the default buffer}
   FreeMem(DefaultBuffer);
  end
 else if Format = COLOR_FORMAT_URGB32 then
  begin
   {If the format is RGB32 (4 bytes per pixel with no alpha channel)}
   {Update the size to be correct for ABGR format}
   Size:=Width * Height * 4;

   {Allocate our actual texture buffer for OpenGL ES}
   Scene.TextureBuffer:=GetMem(Size);
   
   {Convert the image format directly to COLOR_FORMAT_ABGR32 (COLOR_FORMAT_URGB32 is compatible with COLOR_FORMAT_ARGB32)}
   PixelsDefaultToFormat(COLOR_FORMAT_ABGR32,ImageBuffer,Scene.TextureBuffer,Width * Height,False);
   
   {Store the format for our scene}
   Scene.ImageFormat:=GL_RGBA;
   
   {Free the image buffer}
   FreeMem(ImageBuffer);
  end
 else
  begin
   {Free the image buffer if the image is in some other format}
   FreeMem(ImageBuffer);
   Exit;
  end;
 
 {Save the properties of the scene}
 Scene.ImageWidth:=Width;
 Scene.ImageHeight:=Height;
 
 Result:=True;
end;

function GLESIncrementAndWrapAngle(Angle,Increment:GLfloat):GLfloat;
begin
 {}
 {A very simple function to increment the angle of rotation and ensure it wraps around at 360 degrees}
 Angle:=Angle + Increment;

 if Angle >= 360.0 then
  begin
   Angle:=Angle - 360.0;
  end
 else if Angle <=0 then
  begin
   Angle:=Angle + 360.0;
  end;
 
 Result:=Angle;
end;

function GLESIncrementAndClipDistance(Distance,Increment:GLfloat):GLfloat;
begin
 {}
 {Another utility function which increments the distance from the scene and clips within a set range}
 Distance:=Distance + Increment;
 
 if Distance >= 120.0 then
  begin
   Distance:=120.0;
  end
 else if Distance <= 40.0 then
  begin
   Distance:=40.0;
  end;  

 Result:=Distance;  
end;

end.