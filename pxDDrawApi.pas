unit pxDDrawApi;

interface
  uses Windows,
       DirectDraw,
       Forms,
       Graphics;
type
  TSurfaceRestoredEventProc = procedure of object;

  TMyDirectDraw = class
  public
    Constructor create;
    Destructor Destroy;override;
  private
    FDirectDraw     : IDirectDraw;        // DirectDraw object
    FPrimarySurface : IDirectDrawSurface; // DirectDraw primary surface
    FBackSurface    : IDirectDrawSurface; // DirectDraw back surface
    FActive         : Boolean;            // is application active?
    //FPhase          : Byte;
    FMainForm       : TForm;
    //events
    fSurfaceRestoredEventProc :TSurfaceRestoredEventProc;
  protected
    procedure SurfaceRestoredEvent;
  public
    function InitDevice(MainForm:TForm; aWidth, aHeight, BitPerPixel:cardinal):boolean;
    property PrimarySurface:IDirectDrawSurface read FPrimarySurface;
    property BackSurface:IDirectDrawSurface read FBackSurface;
    procedure Flip;

    property OnSurfaceRestored:TSurfacerestoredEventProc read fSurfaceRestoredEventProc write fSurfaceRestoredEventProc;
  end;

implementation

{ TMyDirectDraw }

function TMyDirectDraw.InitDevice(MainForm:TForm; aWidth, aHeight, BitPerPixel: cardinal):boolean;
var
  hr: HRESULT;
  SurfaceDesc: TDDSURFACEDESC ;
  DDSCaps: TDDSCAPS;
begin
  Result := false;
  FMainForm:=MainForm;
  {dandole forma a la forma}
    // FMainForm.BorderStyle:=bsNone;
    // FMainForm.Color:=clBlack;
  {}
  hr := DirectDrawCreate(nil, FDirectDraw, nil);
  if(hr = DD_OK) then begin
    // Get exclusive mode
    hr := FDirectDraw.SetCooperativeLevel(MainForm.Handle, DDSCL_EXCLUSIVE or DDSCL_FULLSCREEN);
    if(hr = DD_OK) then begin
      hr := FDirectDraw.SetDisplayMode(aWidth, aHeight, BitPerPixel);
      if(hr = DD_OK) then begin
        // Create the primary surface with 1 back buffer
        SurfaceDesc.dwSize := sizeof(SurfaceDesc);
        SurfaceDesc.dwFlags := DDSD_CAPS or DDSD_BACKBUFFERCOUNT;
        SurfaceDesc.ddsCaps.dwCaps := DDSCAPS_PRIMARYSURFACE or
                              DDSCAPS_FLIP or
                              DDSCAPS_COMPLEX  or DDSCAPS_SYSTEMMEMORY ;           //con SystemMemory trabaja mucho mas rapido cuando se accede directo a las surface (es mi caso)
        SurfaceDesc.dwBackBufferCount := 1;
        hr := FDirectDraw.CreateSurface(SurfaceDesc, FPrimarySurface, nil);
        if(hr = DD_OK) then begin
          // Get a pointer to the back buffer
          ddscaps.dwCaps := DDSCAPS_BACKBUFFER;
          hr := FPrimarySurface.GetAttachedSurface(ddscaps,
                                                FBackSurface);

              Result := true;
              Exit;
            end;
          end;
        end;
      end;
  MessageBox(MainForm.Handle, PChar(('Fallo Iniciando modo gráfico!')),  'ERROR', MB_OK);
end;

destructor TMyDirectDraw.Destroy;
begin
  if(FDirectDraw <> nil) then
  begin
    FDirectDraw.FlipToGDISurface;
    FDirectDraw.SetCooperativeLevel(FMainForm.Handle, DDSCL_NORMAL);
    if FBackSurface <> nil then
      FBackSurface := nil;
    if FPrimarySurface <> nil then
      FPrimarySurface := nil;
    FDirectDraw := nil;
  end;
  inherited;
end;

procedure TMyDirectDraw.Flip;
var
  hr:HRESULT;
begin

    hr := FPrimarySurface.Flip(nil, DDFLIP_NOVSYNC);
    if(hr = DDERR_SURFACELOST) then
    begin
      hr := FPrimarySurface._Restore();
      SurfaceRestoredEvent;
    end;

end;

constructor TMyDirectDraw.create;
begin
  fSurfaceRestoredEventProc := nil
end;


procedure TMyDirectDraw.SurfaceRestoredEvent;
begin
  if Assigned(fSurfaceRestoredEventProc) then fSurfaceRestoredEventProc();
end;

end.
