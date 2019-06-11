unit DpxFmx;

interface
  uses Dpx, FMX.Surfaces, FMX.Types, FMX.Graphics, System.SysUtils;

type

TDpxFmx = class(TDpx)
  private
    procedure SetSurface(const Value: TBitmapSurface);
  protected
    fSurface :TBitmapSurface;
  public
    {}
    constructor create;
    destructor Destroy;override;
    {}
    procedure Resize(aWidth, aHeight : integer);override;
    procedure UpdateScanlines;override;
    procedure LoadFromFile(const aFileName :string);
    procedure CopyTo( aBitmap:TBitmap );
    {}
    property Surface:TBitmapSurface read fSurface write SetSurface;
end;

implementation

{ TDpxFmx }

procedure TDpxFmx.CopyTo(aBitmap: TBitmap);
var
  data :TBitmapData;
  i :integer;
begin
  //by now only if same dimensions:
  if (aBitmap.Width = fSurface.Width) and (aBitmap.Height = fSurface.Height) then
  begin
    if aBitmap.Map(TMapAccess.Write, data) then
    begin
      for i := 0 to aBitmap.Height-1 do
      begin
        //Converting from BGRA to pixel format of destination bitmap
        AlphaColorToScanline( fSurface.Scanline[i], data.GetScanline(i), aBitmap.Width, aBitmap.PixelFormat );
      end;
      aBitmap.Unmap(data);
    end;
  end
end;

constructor TDpxFmx.create;
begin
  inherited create;
  fSurface := TBitmapSurface.Create;
  ReSize(100,100);
end;

destructor TDpxFmx.Destroy;
begin
  inherited;
  fSurface.Free;
end;

procedure TDpxFmx.LoadFromFile(const aFileName: string);
var
  img :TBitmapSurface;
  img2 :TBitmapSurface;
begin
  img := TBitmapSurface.Create;
    if TBitmapCodecManager.LoadFromFile(aFileName, img) then
    begin
      //convert if pixel format are different
      if fSurface.PixelFormat <> img.PixelFormat then
      begin
        img2 := TBitmapSurface.Create;
        img2.StretchFrom(img, img.Width, img.Height, fSurface.PixelFormat);
        img.Free;
      end else img2 := img;

      //free the old, set the new surface
      fSurface.Free;
      fSurface := img2;
      fHeight := fSurface.Height;
      fWidth := fSurface.Width;
      UpdateScanlines;
    end else img.free;
end;

procedure TDpxFmx.Resize(aWidth, aHeight: integer);
begin
 // if TOSVersion.Platform = pfMacos then
 //   fSurface.SetSize(aWidth, aHeight, TPixelFormat.RGBA)
  //else
  fSurface.SetSize(aWidth, aHeight, TPixelFormat.BGRA);
  inherited;
end;

procedure TDpxFmx.SetSurface(const Value: TBitmapSurface);
begin
  fSurface := Value;
  fWidth := fSurface.Width;
  fHeight := fSurface.Height;
  UpdateScanlines;
end;

procedure TDpxFmx.UpdateScanlines;
var
  i: Integer;
begin
  SetLength(FScanLines, fHeight);
  for i := 0 to FHeight-1 do
    FScanLines[i] := fSurface.Scanline[i];
end;

end.
