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
  windows, Dpx, Graphics, jpeg, sysUtils ;

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
        procedure LoadFromFile( FileName:string );   {Supported formats for TPicture, jpg, bmp, wmf, wme, ico}
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
  begin
    pic := TPicture.Create;
    pic.LoadFromFile(FileName);
    ReSize( pic.Width, pic.Height);
    Bitmap.Canvas.Draw(0,0, pic.Graphic);
    pic.Free;
  end;
end;


procedure TDpxBmp.SetClippingArea(x1, y1, x2, y2: integer);
begin
  inherited;
  if fRegion<>0 then DeleteObject(fRegion);
  fRegion := CreateRectRgn(x1, y1, x2+1, y2+1);
  SelectClipRgn(Bitmap.Canvas.Handle, fRegion)
end;

end.
 