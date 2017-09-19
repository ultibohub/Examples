unit GLES2Unit;

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}
 
{This example is ported from a Blackberry developer tutorial showing how to create a rotating 3D
 cube in OpenGL ES 2.0, the original example can be found here:
 
 https://developer.blackberry.com/native/documentation/getting_started/first_app/3_d_app_draw_the_cube_with_gles20.html
 
 and if you compare it against this you will notice that many things are very similar
 in spite of the different platform.
 
 This means that you should be able to port many OpenGL ES examples to Ultibo by 
 simply changing the platform specific code while keeping most of the OpenGL ES code 
 unchanged.
 
 It is not the purpose of this example to teach you how to use OpenGL ES, for that
 you should consult one of the many online references and tutorials which cover
 everything from the very basic fundamentals to highly advanced topics.
 
 You can find the official Khronos reference for OpenGL ES 2.0 here:
 
 https://www.khronos.org/registry/OpenGL-Refpages/es2.0/

} 

interface

{The VideoCore IV GPU supports both OpenGL ES 1.1 and 2.0 so we have enabled the Free Pascal units
 for both versions in Ultibo. When using OpenGL ES 2.0 the GLES20 unit includes the EGL functions
 needed to setup and initialize OpenGL ES so the only additional requirement is DispmanX}
uses GlobalConst,Console,SysUtils,GLES20,DispmanX,VC4,Math; 
 
const
 {The value of Pi, I'm sure you knew that already}
 M_PI = 3.141592654;
 
const
 {The vertices for the 3D cube (6 sides with 6 vertices per side and 3 values per vertex,
  note that the 6 vertices are required because each side is made up of 2 triangles)}
 Vertices:array[0..(6 * 6 * 3) - 1] of GLfloat = (
  {Front}
  -0.5, 0.5, 0.5,
  -0.5, -0.5, 0.5,
  0.5, 0.5, 0.5,
  0.5, 0.5, 0.5,
  -0.5, -0.5, 0.5,
  0.5, -0.5, 0.5,
  
  {Right}
  0.5, 0.5, 0.5,
  0.5, -0.5, 0.5,
  0.5, 0.5, -0.5,
  0.5, 0.5, -0.5,
  0.5, -0.5, 0.5,
  0.5, -0.5, -0.5,
  
  {Back}
  0.5, 0.5, -0.5,
  0.5, -0.5, -0.5,
  -0.5, 0.5, -0.5,
  -0.5, 0.5, -0.5,
  0.5, -0.5, -0.5,
  -0.5, -0.5, -0.5,
  
  {Left}
  -0.5, 0.5, -0.5,
  -0.5, -0.5, -0.5,
  -0.5, 0.5, 0.5,
  -0.5, 0.5, 0.5,
  -0.5, -0.5, -0.5,
  -0.5, -0.5, 0.5,
  
  {Top}
  -0.5, 0.5, -0.5,
  -0.5, 0.5, 0.5,
  0.5, 0.5, -0.5,
  0.5, 0.5, -0.5,
  -0.5, 0.5, 0.5,
  0.5, 0.5, 0.5,
  
  {Bottom}
  -0.5, -0.5, 0.5,
  -0.5, -0.5, -0.5,
  0.5, -0.5, 0.5,
  0.5, -0.5, 0.5,
  -0.5, -0.5, -0.5,
  0.5, -0.5, -0.5
 );
 
 {The colors for the cube in RGBA format (with each being a floating point value between 0.0 and 1.0)}
 Colors:array[0..(6 * 6 * 4) - 1] of GLfloat = (
  {Front}
  1.0,0.521,0.0,1.0,
  1.0,0.521,0.0,1.0,
  1.0,0.521,0.0,1.0,
  1.0,0.521,0.0,1.0,
  1.0,0.521,0.0,1.0,
  1.0,0.521,0.0,1.0,
  
  {Right}
  1.0,0.0,0.0,1.0,
  1.0,0.0,0.0,1.0,
  1.0,0.0,0.0,1.0,
  1.0,0.0,0.0,1.0,
  1.0,0.0,0.0,1.0,
  1.0,0.0,0.0,1.0,
  
  {Back}
  1.0,1.0,1.0,1.0,
  1.0,1.0,1.0,1.0,
  1.0,1.0,1.0,1.0,
  1.0,1.0,1.0,1.0,
  1.0,1.0,1.0,1.0,
  1.0,1.0,1.0,1.0,
  
  {Left}
  1.0,1.0,0.0,1.0,
  1.0,1.0,0.0,1.0,
  1.0,1.0,0.0,1.0,
  1.0,1.0,0.0,1.0,
  1.0,1.0,0.0,1.0,
  1.0,1.0,0.0,1.0,
  
  {Top}
  0.0,0.7333,0.0,1.0,
  0.0,0.7333,0.0,1.0,
  0.0,0.7333,0.0,1.0,
  0.0,0.7333,0.0,1.0,
  0.0,0.7333,0.0,1.0,
  0.0,0.7333,0.0,1.0,
  
  {Bottom}
  0.752,0.752,0.752,1.0,
  0.752,0.752,0.752,1.0,
  0.752,0.752,0.752,1.0,
  0.752,0.752,0.752,1.0,
  0.752,0.752,0.752,1.0,
  0.752,0.752,0.752,1.0
 );
 
const 
 {OpenGL ES 2.0 uses shaders written in OpenGL Shader Language (GLSL), these are plain text
  pieces of code which are passed to the GPU and compiled at run time.
  
  You will notice the GLSL looks strangely similar to C language, how convenient for them}
 VertexSource:String = 
  'precision mediump float;' +
  'uniform mat4 u_mvpMat;' +
  'attribute vec4 a_position;' +
  'attribute vec4 a_color;' +
  'varying vec4 v_color;' +
  'void main()' +
  '{' +
  '    gl_Position = u_mvpMat * a_position;' +
  '    v_color = a_color;' +
  '}';

 FragmentSource:String = 
  'varying lowp vec4 v_color;' +
  'void main()' +
  '{' +
  '    gl_FragColor = v_color;' +
  '}';
 
type
 {Spatial operations in OpenGL ES 2.0 use matrices for rotation and positioning, here
  we define a simple 4 x 4 matrix record as required by the standard}
 PglMatrix = ^TglMatrix;
 TglMatrix = record
  mat:array[0..3,0..3] of GLfloat;
 end;
 
type
 {A structure to keep track of the overall state including the DispmanX and EGL handles}
 {GLES2 State}
 PGLES2State = ^TGLES2State;
 TGLES2State = record
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
  Alpha:VC_DISPMANX_ALPHA_T;
  NativeWindow:EGL_DISPMANX_WINDOW_T;
  ConfigAttributes:array[0..10] of EGLint;
  ContextAttributes:array[0..2] of EGLint;
 end;
 
 {Another structure to track the details of our OpenGL ES scene}
 {GLES2 Scene}
 PGLES2Scene = ^TGLES2Scene;
 TGLES2Scene = record
  {Data}
  programID:GLuint;
  vertexID:GLuint;
  colorID:GLuint;
  {Locations}
  mvpLoc:GLuint;
  positionLoc:GLuint;
  colorLoc:GLuint;
  {Matrices}
  projectionMat:PglMatrix;
  modelviewMat:PglMatrix;
  mvpMat:PglMatrix;
 end;
 
{The main functions of our GLES2 example} 
procedure StartGLES2;
procedure CleanupGLES2(var State:TGLES2State;var Scene:TGLES2Scene);
 
{Functions dealing with initializing, updating and rendering our OpenGL ES scene}
function GLES2Init(var State:TGLES2State):Boolean;

function GLES2InitScene(var State:TGLES2State;var Scene:TGLES2Scene):Boolean;
procedure GLES2RenderScene(var State:TGLES2State;var Scene:TGLES2Scene);

{Some utility functions to handle matrix operations}
procedure GLES2MultiplyMatrix(Result,SourceA,SourceB:PglMatrix);
procedure GLES2LoadIdentity(Result:PglMatrix);
procedure GLES2ScaleMatrix(Result:PglMatrix;sx,sy,sz:GLfloat);
procedure GLES2RotateMatrix(Result:PglMatrix;Angle,x,y,z:GLfloat);
procedure GLES2FrustumMatrix(Result:PglMatrix;Left,Right,Bottom,Top,NearZ,FarZ:GLfloat);

implementation

procedure StartGLES2;
var
 State:TGLES2State;
 Scene:TGLES2Scene;
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
 FillChar(State,SizeOf(TGLES2State),0);
 FillChar(Scene,SizeOf(TGLES2Scene),0);
 
 {Initialize the OpenGL ES 2.0 state}
 GLES2Init(State);
 
 {Initialize the OpenGL ES 2.0 scene}
 if not GLES2InitScene(State,Scene) then Exit;

 {Empty the keyboard buffer before starting our loop}
 while ConsoleKeypressed do
  begin
   ConsoleReadKey;
  end;
 
 while True do
  begin
   {Render the scene and display again}
   GLES2RenderScene(State,Scene);
   
   {Check for a keypress and break if found}
   if ConsoleKeyPressed then Break;
  end;
  
 {Cleanup the OpenGL ES 2.0 state and scene} 
 CleanupGLES2(State,Scene);
 
 {Deinitialize the VC4}
 BCMHostDeinit;
end;

procedure CleanupGLES2(var State:TGLES2State;var Scene:TGLES2Scene);
var
 Success:Integer;
 DispmanUpdate:DISPMANX_UPDATE_HANDLE_T;
begin
 {}
 {Clear the screen}
 glClear(GL_COLOR_BUFFER_BIT);
 eglSwapBuffers(State.Display,State.Surface);
 
 {Delete the OpenGL ES buffers}
 glDeleteBuffers(1,@Scene.vertexID);
 glDeleteBuffers(1,@Scene.colorID);
 
 {Delete the OpenGL ES program}
 glDeleteProgram(Scene.programID);
 
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
  
 {Release the matrices}
 FreeMem(Scene.projectionMat);
 FreeMem(Scene.modelviewMat);
 FreeMem(Scene.mvpMat);
end;

function GLES2Init(var State:TGLES2State):Boolean;
var
 Config:EGLConfig;
 ConfigCount:EGLint;
 EGLResult:EGLBoolean;

 DestRect:VC_RECT_T;
 SourceRect:VC_RECT_T;
 DispmanUpdate:DISPMANX_UPDATE_HANDLE_T;
begin
 {}
 {This function provides a good general outline of how to initialize OpenGL ES 2.0 on the
  VideoCore IV and obtain the necessary handles and contexts.
  
  You will need something similar to this in all OpenGL ES applications before you can
  perform any OpenGL ES operations. 
  
  For an OpenGL ES 1.1 version of this function please see the HelloGLES example}
 Result:=False;
 
 {Setup some DispmanX and EGL defaults}
 State.DispmanDisplay:=DISPMANX_NO_HANDLE;
 State.DispmanElement:=DISPMANX_NO_HANDLE;
 DispmanUpdate:=DISPMANX_NO_HANDLE;
 State.Display:=EGL_NO_DISPLAY;
 State.Surface:=EGL_NO_SURFACE;
 State.Context:=EGL_NO_CONTEXT;
 
 {Setup the alpha channel state}
 State.Alpha.flags:=DISPMANX_FLAGS_ALPHA_FIXED_ALL_PIXELS;
 State.Alpha.opacity:=255;
 State.Alpha.mask:=0;
 
 {Setup the EGL configuration attributes}
 State.ConfigAttributes[0]:=EGL_RENDERABLE_TYPE;
 State.ConfigAttributes[1]:=EGL_OPENGL_ES2_BIT;
 State.ConfigAttributes[2]:=EGL_SURFACE_TYPE;
 State.ConfigAttributes[3]:=EGL_WINDOW_BIT;
 State.ConfigAttributes[4]:=EGL_BLUE_SIZE;
 State.ConfigAttributes[5]:=8;
 State.ConfigAttributes[6]:=EGL_GREEN_SIZE;
 State.ConfigAttributes[7]:=8;
 State.ConfigAttributes[8]:=EGL_RED_SIZE;
 State.ConfigAttributes[9]:=8;
 State.ConfigAttributes[10]:=EGL_NONE;
 
 {Setup the EGL context attributes}
 State.ContextAttributes[0]:=EGL_CONTEXT_CLIENT_VERSION;
 State.ContextAttributes[1]:=2;
 State.ContextAttributes[2]:=EGL_NONE;
 
 try
  {Get an EGL display connection}
  State.Display:=eglGetDisplay(EGL_DEFAULT_DISPLAY);
  if State.Display = EGL_NO_DISPLAY then Exit;
  
  {Initialize the EGL display connection}
  EGLResult:=eglInitialize(State.Display,nil,nil);
  if EGLResult = EGL_FALSE then Exit;
  
  {Get an appropriate EGL framebuffer configuration}
  EGLResult:=eglChooseConfig(State.Display,@State.ConfigAttributes,@Config,1,@ConfigCount);
  if EGLResult = EGL_FALSE then Exit;
  
  {Bind to the OpenGL ES API}
  EGLResult:=eglBindAPI(EGL_OPENGL_ES_API);
  if EGLResult = EGL_FALSE then Exit;
  
  {Create an EGL rendering context}
  State.Context:=eglCreateContext(State.Display,Config,EGL_NO_CONTEXT,@State.ContextAttributes);
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
  State.DispmanElement:=vc_dispmanx_element_add(DispmanUpdate,State.DispmanDisplay,0 {Layer},@DestRect,0 {Source},@SourceRect,DISPMANX_PROTECTION_NONE,@State.Alpha,nil {Clamp},DISPMANX_NO_ROTATE {Transform});
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
  
  {Preserve the buffers on swap}
  EGLResult:=eglSurfaceAttrib(State.Display,State.Surface,EGL_SWAP_BEHAVIOR,EGL_BUFFER_PRESERVED);
  if EGLResult = EGL_FALSE then Exit;
 
  {Connect the EGL context to the EGL surface}
  EGLResult:=eglMakeCurrent(State.Display,State.Surface,State.Surface,State.Context);
  if EGLResult = EGL_FALSE then Exit;
  
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

function GLES2InitScene(var State:TGLES2State;var Scene:TGLES2Scene):Boolean;
var
 aspect:GLfloat;
 _near:GLfloat;
 _far:GLfloat;
 yFOV: GLfloat;
 height:GLfloat;
 width:GLfloat;

 Status:GLint;
 Source:PChar;
 VertexShader:GLuint;
 FragmentShader:GLuint;
 Log:array[0..255] of Char;
begin
 {}
 {This function initializes the 3D cube as well as the OpenGL ES shaders and program,
  for more information please see the OpenGL ES 2.0 documentation}
 Result:=False;

 {Clear depth, color, background and enable back face culling} 
 glClearDepthf(1.0);
 
 glClearColor(0.0,0.0,0.0,1.0);
 
 glEnable(GL_CULL_FACE);
 
 {Create, upload and compile the vertex shader}
 VertexShader:=glCreateShader(GL_VERTEX_SHADER);
 if VertexShader = 0 then
  begin
   ConsoleWriteLn('Error: Failed to create vertex shader (Error=' + IntToStr(glGetError) + ')');
   Exit;
  end
 else
  begin
   Source:=PChar(VertexSource);
   glShaderSource(VertexShader,1,@Source,nil); {Note: PPChar = Address of the PChar (String) not the PChar itself}
   glCompileShader(VertexShader);
   glGetShaderiv(VertexShader,GL_COMPILE_STATUS,@Status);
   if Status = GL_FALSE then
    begin
     glGetShaderInfoLog(VertexShader,256,nil,@Log);
     
     ConsoleWriteLn('Error: Failed to compile vertex shader (Error=' + Log + ')');
     
     glDeleteShader(VertexShader);
     
     Exit;
    end;
  end;
 
 {Create, upload and compile the fragment shader} 
 FragmentShader:=glCreateShader(GL_FRAGMENT_SHADER);
 if FragmentShader = 0 then
  begin
   ConsoleWriteLn('Error: Failed to create fragment shader (Error=' + IntToStr(glGetError) + ')');
   Exit;
  end
 else
  begin
   Source:=PChar(FragmentSource);
   glShaderSource(FragmentShader,1,@Source,nil); {Note: PPChar = Address of the PChar (String) not the PChar itself}
   glCompileShader(FragmentShader);
   glGetShaderiv(FragmentShader,GL_COMPILE_STATUS,@Status);
   if Status = GL_FALSE then
    begin
     glGetShaderInfoLog(FragmentShader,256,nil,@Log);
     
     ConsoleWriteLn('Error: Failed to compile fragment shader (Error=' + Log + ')');
     
     glDeleteShader(VertexShader);
     glDeleteShader(FragmentShader);
     
     Exit;
    end;
  end;
 
 {Create and link the program}
 Scene.programID:=glCreateProgram;
 if Scene.programID = 0 then
  begin
   ConsoleWriteLn('Error:Failed to create a shader program');
   Exit;
  end
 else
  begin
   glAttachShader(Scene.programID,VertexShader);
   glAttachShader(Scene.programID,FragmentShader);
   glLinkProgram(Scene.programID);

   glGetProgramiv(Scene.programID,GL_LINK_STATUS,@Status);
   if Status = GL_FALSE then
    begin
     glGetProgramInfoLog(Scene.programID,256,nil,@Log);
   
     ConsoleWriteLn('Error: Failed to link shader program (Error=' + Log + ')');
     
     glDeleteProgram(Scene.programID);
     
     Exit;
    end;
  end;
  
 {Discard the shaders which are not required now they are linked to the program}
 glDeleteShader(FragmentShader);
 glDeleteShader(VertexShader);
 
 {Obtain the locations of some uniforms and attributes from our shaders}
 Scene.mvpLoc:=glGetUniformLocation(Scene.programID,'u_mvpMat');
 Scene.positionLoc:=glGetAttribLocation(Scene.programID,'a_position');
 Scene.colorLoc:=glGetAttribLocation(Scene.programID,'a_color');
 
 {Generate vertex and color buffers and fill them with our cube data}
 glGenBuffers(1,@Scene.vertexID);
 glBindBuffer(GL_ARRAY_BUFFER,Scene.vertexID);
 glBufferData(GL_ARRAY_BUFFER,SizeOf(Vertices),@Vertices,GL_STATIC_DRAW);
 
 glGenBuffers(1,@Scene.colorID);
 glBindBuffer(GL_ARRAY_BUFFER,Scene.colorID);
 glBufferData(GL_ARRAY_BUFFER,SizeOf(Colors),@Colors,GL_STATIC_DRAW);
 
 {Allocate and initialize the projection matrix}
 Scene.projectionMat:=GetMem(SizeOf(TglMatrix));
 GLES2LoadIdentity(Scene.projectionMat);
 
 {Calculate the frustum and scale the projection}
 aspect:=State.ScreenWidth / State.ScreenHeight;
 _near:=-2.0;
 _far:=2.0;
 yFOV:=75.0;
 height:=tan(yFOV / 360.0 * M_PI) * _near;
 width:=height * aspect;
 
 GLES2FrustumMatrix(Scene.projectionMat,-width,width,-height,height,_near,_far);

 if State.ScreenWidth > State.ScreenHeight then
  begin
   GLES2ScaleMatrix(Scene.projectionMat,State.ScreenHeight / State.ScreenWidth,1.0,1.0);
  end
 else
  begin
   GLES2ScaleMatrix(Scene.projectionMat,1.0,State.ScreenWidth / State.ScreenHeight,1.0);
  end;  
  
 {Allocate and initialize the other matrices} 
 Scene.modelviewMat:=GetMem(SizeOf(TglMatrix));
 GLES2LoadIdentity(Scene.modelviewMat);
 Scene.mvpMat:=GetMem(SizeOf(TglMatrix));
 
 Result:=True;
end;

procedure GLES2RenderScene(var State:TGLES2State;var Scene:TGLES2Scene);
begin
 {}
 {Update the position of our 3D cube model, render it and swap buffers
  in order to display it again.
 
  To understand what is happening you should consult a good OpenGL ES 2.0
  tutorial and the official documentation}
 
 {Apply the viewport to the entire screen}
 glViewport(0,0,State.ScreenWidth,State.ScreenHeight);
 
 {Clear our scene}
 glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
 
 {Enable our program}
 glUseProgram(Scene.programID);
 
 {Enable and bind the vertex information}
 glEnableVertexAttribArray(Scene.positionLoc);
 glBindBuffer(GL_ARRAY_BUFFER,Scene.vertexID);
 glVertexAttribPointer(Scene.positionLoc,3,GL_FLOAT,GL_FALSE,3 * SizeOf(GLfloat),nil);
 
 {Enable and bind the color information}
 glEnableVertexAttribArray(Scene.colorLoc);
 glBindBuffer(GL_ARRAY_BUFFER,Scene.colorID);
 glVertexAttribPointer(Scene.colorLoc,4,GL_FLOAT,GL_FALSE,4 * SizeOf(GLfloat),nil);
 
 {Rotate the model}
 GLES2RotateMatrix(Scene.modelviewMat,1.0,-0.374,-0.608,0.923);
 
 GLES2MultiplyMatrix(Scene.mvpMat,Scene.modelviewMat,Scene.projectionMat);
 glUniformMatrix4fv(Scene.mvpLoc,1,GL_FALSE,@Scene.mvpMat.mat[0][0]);
 
 {Draw all of our triangles at once}
 glDrawArrays(GL_TRIANGLES,0,36);
 
 {Disable the attribute arrays}
 glDisableVertexAttribArray(Scene.positionLoc);
 glDisableVertexAttribArray(Scene.colorLoc);
 
 {Swap the buffers to display the new scene}
 eglSwapBuffers(State.Display,State.Surface);
end;

procedure GLES2MultiplyMatrix(Result,SourceA,SourceB:PglMatrix);
var
 Count:Integer;
 Temp:TglMatrix;
begin
 {}
 {A utility function to perform a similar operation to the glMultMatrixf function
  from OpenGL ES 1.1}
 for Count:=0 to 3 do
  begin
   Temp.mat[Count][0]:=(SourceA.mat[Count][0] * SourceB.mat[0][0]) + (SourceA.mat[Count][1] * SourceB.mat[1][0]) + (SourceA.mat[Count][2] * SourceB.mat[2][0]) + (SourceA.mat[Count][3] * SourceB.mat[3][0]);
   Temp.mat[Count][1]:=(SourceA.mat[Count][0] * SourceB.mat[0][1]) + (SourceA.mat[Count][1] * SourceB.mat[1][1]) + (SourceA.mat[Count][2] * SourceB.mat[2][1]) + (SourceA.mat[Count][3] * SourceB.mat[3][1]);
   Temp.mat[Count][2]:=(SourceA.mat[Count][0] * SourceB.mat[0][2]) + (SourceA.mat[Count][1] * SourceB.mat[1][2]) + (SourceA.mat[Count][2] * SourceB.mat[2][2]) + (SourceA.mat[Count][3] * SourceB.mat[3][2]);
   Temp.mat[Count][3]:=(SourceA.mat[Count][0] * SourceB.mat[0][3]) + (SourceA.mat[Count][1] * SourceB.mat[1][3]) + (SourceA.mat[Count][2] * SourceB.mat[2][3]) + (SourceA.mat[Count][3] * SourceB.mat[3][3]);
  end;
  
 System.Move(Temp,Result^,SizeOf(TglMatrix));
end;

procedure GLES2LoadIdentity(Result:PglMatrix);
begin
 {}
 {Load a matrix with the identity matrix values which is equivalent to the
  glLoadIdentity function in OpenGL ES 1.1}
 FillChar(Result^,SizeOf(TglMatrix),0);
 
 Result.mat[0][0]:=1.0;
 Result.mat[1][1]:=1.0;
 Result.mat[2][2]:=1.0;
 Result.mat[3][3]:=1.0;
end;

procedure GLES2ScaleMatrix(Result:PglMatrix;sx,sy,sz:GLfloat);
begin
 {}
 {Scale the supplied matrix structure}
 Result.mat[0][0]:=Result.mat[0][0] * sx;
 Result.mat[0][1]:=Result.mat[0][1] * sx;
 Result.mat[0][2]:=Result.mat[0][2] * sx;
 Result.mat[0][3]:=Result.mat[0][3] * sx;

 Result.mat[1][0]:=Result.mat[1][0] * sy;
 Result.mat[1][1]:=Result.mat[1][1] * sy;
 Result.mat[1][2]:=Result.mat[1][2] * sy;
 Result.mat[1][3]:=Result.mat[1][2] * sy;

 Result.mat[2][0]:=Result.mat[2][0] * sz;
 Result.mat[2][1]:=Result.mat[2][1] * sz;
 Result.mat[2][2]:=Result.mat[2][2] * sz;
 Result.mat[2][3]:=Result.mat[2][3] * sz;
end;

procedure GLES2RotateMatrix(Result:PglMatrix;Angle,x,y,z:GLfloat);
var
 sinAngle:GLfloat;
 cosAngle:GLfloat;
 mag:GLfloat;
 
 xx:GLfloat;
 yy:GLfloat;
 zz:GLfloat;
 xy:GLfloat;
 yz:GLfloat;
 zx:GLfloat;
 xs:GLfloat;
 ys:GLfloat;
 zs:GLfloat;
 
 oneMinusCos:GLfloat;
 Rotation:TglMatrix;
begin
 {}
 {Rotate the supplied matrix structure, similar in function to glRotatef from OpenGL ES 1.1}
 mag:=sqrt(x * x + y * y + z * z);
 
 sinAngle:=sin(Angle * M_PI / 180.0);
 cosAngle:=cos(Angle * M_PI / 180.0);

 if mag > 0.0 then
  begin
   x:=x / mag;
   y:=y / mag;
   z:=z / mag;
 
   xx:=x * x;
   yy:=y * y;
   zz:=z * z;
   xy:=x * y;
   yz:=y * z;
   zx:=z * x;
   xs:=x * sinAngle;
   ys:=y * sinAngle;
   zs:=z * sinAngle;
   oneMinusCos:=1.0 - cosAngle;
 
   Rotation.mat[0][0]:=(oneMinusCos * xx) + cosAngle;
   Rotation.mat[0][1]:=(oneMinusCos * xy) - zs;
   Rotation.mat[0][2]:=(oneMinusCos * zx) + ys;
   Rotation.mat[0][3]:=0.0;

   Rotation.mat[1][0]:=(oneMinusCos * xy) + zs;
   Rotation.mat[1][1]:=(oneMinusCos * yy) + cosAngle;
   Rotation.mat[1][2]:=(oneMinusCos * yz) - xs;
   Rotation.mat[1][3]:=0.0;

   Rotation.mat[2][0]:=(oneMinusCos * zx) - ys;
   Rotation.mat[2][1]:=(oneMinusCos * yz) + xs;
   Rotation.mat[2][2]:=(oneMinusCos * zz) + cosAngle;
   Rotation.mat[2][3]:=0.0;

   Rotation.mat[3][0]:=0.0;
   Rotation.mat[3][1]:=0.0;
   Rotation.mat[3][2]:=0.0;
   Rotation.mat[3][3]:=1.0;

   GLES2MultiplyMatrix(Result,@Rotation,Result);
  end;      
end;

procedure GLES2FrustumMatrix(Result:PglMatrix;Left,Right,Bottom,Top,NearZ,FarZ:GLfloat);
var
 DeltaX:GLfloat;
 DeltaY:GLfloat;
 DeltaZ:GLfloat;
 Frust:TglMatrix;
begin
 {}
 {A utility function to frustum a matrix structure, as per glFrustumf from OpenGL ES 1.1}
 DeltaX:=Right - Left;
 DeltaY:=Top - Bottom;
 DeltaZ:=FarZ - NearZ;

 if (NearZ <= 0.0) or (FarZ <= 0.0) or (DeltaX <= 0.0) or (DeltaY <= 0.0) or (DeltaZ <= 0.0) then Exit;
 
 Frust.mat[0][0]:=2.0 * NearZ / DeltaX;
 Frust.mat[0][1]:=0.0;
 Frust.mat[0][2]:=0.0;
 Frust.mat[0][3]:=0.0;
 
 Frust.mat[1][1]:=2.0 * NearZ / DeltaY;
 Frust.mat[1][0]:=0.0;
 Frust.mat[1][2]:=0.0;
 Frust.mat[1][3]:=0.0;
 
 Frust.mat[2][0]:=(Right + Left) / DeltaX;
 Frust.mat[2][1]:=(Top + Bottom) / DeltaY;
 Frust.mat[2][2]:=-(NearZ + FarZ) / DeltaZ;
 Frust.mat[2][3]:=-1.0;
 
 Frust.mat[3][2]:=-2.0 * NearZ * FarZ / DeltaZ;
 Frust.mat[3][0]:=0.0;
 Frust.mat[3][1]:=0.0;
 Frust.mat[3][3]:=0.0;
 
 GLES2MultiplyMatrix(Result,@Frust,Result);
end;

end.