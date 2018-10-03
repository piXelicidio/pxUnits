unit DpxBmp;
{
UNIT: DpxBmp
AUTHOR: Denys Almaral Rodríguez (piXel)
EMAIL: pxtracer@gmail.com
DATE: may-2009
LAST MODIFIED: mayo-2009

Descripción:
  Implementa la versión para Bitmap de windows y Delphi VCL, para trabajo
  en windows y con delphi de la biblioteca gráfica Dpx
  Contenido (Resumen):
  -Tranferencia de imagen para dibujar sobre ventanas con BitBlt
  -Salvar a Bmp y cargar de todos los formatos soportados por Delphi incluyendo el Jpg!

  TODO
  *Salvar a TGA
}
interface

uses
  windows, Dpx, Graphics, jpeg, sysUtils, pngimage ;

type
      TRectsArray = array of TRect;

      TDpxBmp = class(TDpx)
      public
        constructor Create;
        destructor Destroy;override;
      private
         FBackBuffer :TBitmap;
         fRegion  :HRGN;
      protected
      public
        procedure ReSize( aWidth, aHeight:integer );override;
        procedure SetClippingArea( x1,y1, x2,y2 :integer);override;
        procedure UpdateScanlines;override;         //any change of bitmap dimensions you most call this proc
        //sytem transference
        procedure Show(DC:HDC);overload;
        procedure Show(DC:HDC; x,y :integer);overload;
        procedure ShowRect(DC:HDC; var Rect:TRect );overload;
        procedure ShowRect(DC:HDC; x1,y1,x2,y2 :integer);overload;
        procedure ShowRect(DC:HDC; x,y, x1,y1,x2,y2 :integer);overload;
        procedure ShowRects(DC:HDC; var rects:TRectsArray);

        procedure Copy(DC:HDC);

        {files loading}
        procedure SaveToBMP( FileName:string );
        procedure SaveToPNG( FileName:string );
        procedure LoadFromPNG( FileName:string );
        procedure LoadFromFile( FileName:string );   {TGA32, and Supported formats for TPicture, PNG, jpg, bmp, wmf, wme, ico}
        property Bitmap:TBitmap read FBackBuffer;
      end;

implementation
{TDpxBmp}

constructor TDpxBmp.Create;
begin
  inherited;
  FBackBuffer:=TBitmap.Create;
  UpdateScanLines;
end;

destructor TDpxBmp.Destroy;
begin
  FBackBuffer.Free;
  if fRegion<>0 then DeleteObject(fRegion);
  inherited;
end;


procedure TDpxBmp.UpdateScanlines;
var
  i:integer;
begin
  Bitmap.pixelFormat:=pf32bit;
  SetLength(FScanLines, Bitmap.height);
  FWidth := Bitmap.Width;
  FHeight := Bitmap.Height;
  for i:=0 to Bitmap.Height-1 do FScanlines[i]:=Bitmap.ScanLine[i];
end;


procedure TDpxBmp.Show(DC: HDC);
begin
  bitblt(DC,0,0,FBackBuffer.Width, FBackBuffer.Height,
         FBackBuffer.canvas.Handle, 0, 0, srcCopy);
end;

procedure TDpxBmp.SaveToBMP(FileName: string);
begin
  Bitmap.PixelFormat := pf24Bit;
  Bitmap.SaveToFile(FileName);
  Bitmap.PixelFormat := pf32Bit;
  UpdateScanLines;
end;

procedure TDpxBmp.SaveToPNG(FileName: string);
var
  myPNG :TPngImage;
  i,j :integer;
  p,pa :PByte;
  c:TRGBA;
begin
  myPNG := TPngImage.CreateBlank(COLOR_RGBALPHA, 8, FWidth, FHeight );

  for j := 0 to FHeight-1 do
  begin
    p := myPng.Scanline[j];
    pa := PBYTE(myPng.AlphaScanline[j]);
    for i := 0 to FWidth-1 do
    begin
      c.color := GetPixel(i,j);
      p^ := c.r ;
      inc(p);
      p^ := c.g ;
      inc(p);
      p^ := c.b;

      pa^ := c.a;
      inc(p);
      inc(pa);
    end;

  end;

  myPNG.SaveToFile(FileName);
  myPNG.Free;
end;

procedure TDpxBmp.Show(DC: HDC; x, y: integer);
begin
     BitBlt(DC,x,y,FBackBuffer.Width, FBackBuffer.Height,
         FBackBuffer.canvas.Handle, 0, 0, srcCopy);
end;

procedure TDpxBmp.ShowRect(DC: HDC; x1, y1, x2, y2: integer);
begin
 BitBlt( DC,
          x1,  y1,
          x2 - x1 ,
          y2 - y1 ,
          Bitmap.canvas.handle,
          x1, y1, SRCCOPY)
end;

procedure TDpxBmp.ShowRect(DC: HDC; x, y, x1, y1, x2, y2: integer);
begin
  BitBlt( DC,
          x, y,
          x2 - x1 ,
          y2 - y1 ,
          Bitmap.canvas.handle,
          x1, y1, SRCCOPY)
end;

procedure TDpxBmp.ShowRect(DC: HDC; var Rect: TRect);
begin
with rect do
  BitBlt( DC,
          left, top,
          right - left ,
          bottom - top ,
          Bitmap.canvas.handle,
          left, top, SRCCOPY)
end;

procedure TDpxBmp.ShowRects(DC: HDC; var rects: TRectsArray);
var
  cant,i:integer;
begin
  cant:=length(rects);
  for i:=0 to cant-1 do
  begin
    with rects[i] do
      BitBlt( DC,
            left, top,
            right - left ,
            bottom - top ,
            Bitmap.canvas.handle,
            left, top, SRCCOPY)
  end;
end;


procedure TDpxBmp.Copy(DC: HDC);
begin
  bitblt(FBackBuffer.Canvas.Handle,0,0,FBackBuffer.Width, FBackBuffer.Height,
         DC, 0, 0, srcCopy);
end;


procedure TDpxBmp.ReSize(aWidth, aHeight: integer);
begin
  FBackBuffer.Width := aWidth;
  FBackBuffer.Height := aHeight;
  inherited ReSize(aWidth, aHeight);
end;


procedure TDpxBmp.LoadFromFile(FileName: string);
var
  pic :TPicture;
  s :string;
begin
  s :=ExtractfileExt(FileName);
  if s='.tga' then LoadFromTGA(FileName) else
    if s='.png' then LoadFromPNG(FileName) else
    begin
      pic := TPicture.Create;
      pic.LoadFromFile(FileName);
      ReSize( pic.Width, pic.Height);
      Bitmap.Canvas.Draw(0,0, pic.Graphic);
      pic.Free;
    end;
end;


procedure TDpxBmp.LoadFromPNG(FileName: string);
//asuming PNG 24 bits + AlphaChannel. TODO:FIX for NON-alpha Info dude
var
  pic :TPicture;
  thePng :TPngImage;
  i,j :integer;
  p :PByte;  //pixel RGB
  pa :PByte; //pixel Alpha
  c :TRGBA;

begin
  pic := TPicture.Create;
  pic.LoadFromFile(FileName);
  thePng := pic.Graphic as TPngImage;
  ReSize( pic.Width, pic.Height);
   CLS($0);

  for j := 0 to pic.Height-1 do
  begin
    p := thePng.Scanline[j];
    pa := PBYTE(thePng.AlphaScanline[j]);
    for i := 0 to pic.width-1 do
    begin
      c.r := p^;
      inc(p);
      c.g := p^;
      inc(p);
      c.b := p^;

      c.a:=pa^;
      inc(p);
      inc(pa);

      PutPixel(i,j,c.color);
    end;

  end;
//      Bitmap.Canvas.Draw(0,0, pic.Graphic);
  pic.Free;
end;

procedure TDpxBmp.SetClippingArea(x1, y1, x2, y2: integer);
begin
  inherited;
  if fRegion<>0 then DeleteObject(fRegion);
  fRegion := CreateRectRgn(x1, y1, x2+1, y2+1);
  SelectClipRgn(Bitmap.Canvas.Handle, fRegion)
end;

end.
 