unit Dpx;
{
UNIT: Dpx - DirectPixel32                         (32 bits only)
AUTHOR: Denys Almaral Rodríguez (piXel)
EMAIL: pxtracer@gmail.com
started: jun-2004
LAST MODIFIED: december-2012

Descripción:
  Clase para trabajo con pixeles directo en memoria.
  TDpx, es una clase abstracta  para trabajar sobre cualquier
  memoria que almacene pixeles de 32 bits.

  Contenido (Resumen):
    Manejo de píxeles -> PutPixel, PutImage, FillRect...
    -Clipping -> Casi todas los procedimientos tienen una version Clip...
                dada una Area definida con SetClippingArea.
    -Alpha Blending > !con optimizaciones interesantes!!
    -Mask, Keyinng.
    -Interpolación de pixeles.
    -Scroll
    -Carga de imagenes TGA con y sin compression RLE.

    -TOptSptrite Objeto que contiene imagen optimizada con alphablending...

    -TDpxMem: Un Dpx creado en memoria de sistema

    -TDpxRef: Un Dpx que se adihere a otro, referencia la imagen, no tiene memoria propia

}

//  HISTORY:
{
  - (18/12/12) Reorganize and Re-Name Methods <-- CRITIC CHANGE
  - (12/12) upgrade To Delphi XE3
  -  Versión de LoadFromTGA que en véz de fichero en disco
         lea el TGA de un Puntero en Memoria.
 }

 //TODO: Better naming convetion
 //TODO: Translate all to English
 //TODO: Improve TGA Loading with official Specs  (support 24bits other..)



interface
//uses    Windows;   USES NOTHING! SYSTEM.PAS I THINK


const
  cxBlack = $0;
  cxWhite = $FFffFF;
  cxBlue  = $ff;
  cxRed   = $ff0000;
  cxGreen = $ff00;
  cxOpaque = $ff000000;
  cxGray  = $808080;


Type
      {Estructura para a través de los punteros
      que RefresScanLines actualiza acceder a cada
      informacón de pixel de 32 bits individualmente}
      PDWordArray=^TDwordArray;
      TDWordArray = array[0..32767] of LongWord;

      {Estructura para guardar informacion de
      segmentos de máscaras, o sea posiciónes continuas de pixeles}

      TPixelSegm = record
        x, y, cantx4 :integer;
      end;

      TPixelSegments = array of TPixelSegm;

      PRGBA = ^TRGBA;
      TRGBA = record
        case integer of
            0: (color   :LongWord );
            1: (r,g,b,a :byte );
      end;

      PPt = ^TPt;
      TPt = record
        x,y :integer
      end;

      TPtArray = array of TPt;

      PRct  = ^TRct;
      TRct = record
        case integer of
        0:(
        x1,y1,
        x2,y2 :integer);
        1:(
        P1,P2 :TPt);
      end;


      {Encabezamiento del TGA}
      TTGAHeader = packed record
        UnknownData0  :word;
        Compression   :byte;
        UnknownData   :array[0..8] of byte;
        Width         :word;
        Height        :word;
        UnknownData2  :word;
      end;

      TPixelTestFunc = function( c:LongWord ):boolean of object;
      TPutPixelProc = procedure( x, y :integer; c:LongWord ) of object;

      TOptSprite = class;

      TDpx = Class
        private
          fAlphaTransparentCoords :TPixelSegments;                    //posiciones lineas con alpha blending generadas por ReCalculateAlphaMaskCoords
          fAlphaOpaqueCoords      :TPixelSegments;                    //posiciones de lineas opacas generadas ReCalculateAlphaMaskCoords
          fMaskCoords             :TPixelSegments;                    //posiciones de lineas opacas generadas por  UpdateMaskCoords
          bkColorTmp     :LongWord;
          minAlphaTmp    :byte;
          maxAlphaTmp    :byte;
          fAlphaOptimized :boolean;
        protected
          FWidth ,                          {abstract ini} // estos campos deben ser actualizados por clases
          FHeight    :integer;              {abstract ini} // susesoras cada vez que se modifiquen las dimensiones
          FScanLines :array of PDWordArray; {abstract ini} // de la imagen, la localizacion en memoria //Cambiar a PDWordArray, pa que pointer??????
          FX1Clip, FX2Clip,                 //clipping Area Tambien actualiar
          FY1Clip, FY2Clip  :integer;       //clipping Area Tambien actualizar
          fImgBoundingRect  :TRct;          //Rectángulo que rodea la imagen analizada por PreAlphaOptimize
          function PixelTestGMC(c:LongWord):boolean;                          //funcion de condicion para pasarle a GetLineCoords
          function PixelTestTransparent(c:LongWord):boolean;                  // ..
          function PixelTestOpaque(c:LongWord):boolean;                       // ..
          procedure xLineCutting( x1, x2:integer; var xCut1, xCut2:integer);  //clipping a lineas horizontales
          procedure yLineCutting( y1, y2:integer; var yCut1, yCut2:integer);  //clippinga a lineas verticales
          function CalcBoundingRect( ca :TPixelSegments ):TRct;                     //halla el boundingRect segun los TPixelSegments de optimización
        public
          {inicializacionnes}
          constructor Create;
          destructor destroy;override;
          {Abstracts}
          procedure UpdateScanlines;virtual;abstract;
          procedure ReSize( aWidth, aHeight:integer );virtual;                //Update here Width/Height ScanLines pointers;
          {Low-Level Access }
          function GetScanLinePtr( y: integer ):pointer;
          function GetScanLinePtrA( y: integer ):PDWordArray;
          function GetPtr( x,y:integer ):pointer;
          function GetPtrA( x,y:integer ):PDWordArray;
          {PutPixels}
          procedure PutPixel(x, y:integer; c:LongWord );                     //oh!! the epic putpixel!!!
          procedure PutPixelClip(x,y:integer; c:LongWord );                  //clipped putpixel
          procedure PutPixelTile(x,y:integer; c:LongWord );                  //Pixel Mosaico, si las coordenadas se salen del clipping area el pixel se repite cíclicamente... lo cojes?
          function  GetPixel(x, y:integer ):LongWord;                        //oh!! el getpixel
          function  GetPixelClip(x,y:integer):LongWord;
          function  GetPixelTile(x,y:integer):LongWord;                      //..
          procedure PutPixelbuffer(x, y:integer; var buffer; pixelsCount:integer); //dibuja un buffer de pixeles...
          function  GetLineCoords(condition: TPixelTestFunc ):TPixelSegments;    //Detecta lineas continuas de píxeles que cumplan la condicion
          {MiscGraphics}
          procedure CLS(c:longWord);                                          //oh! el viejo Clear Screen... rellena todo de color entero C
          procedure SetClippingArea( x1,y1, x2,y2 :integer);virtual;          //Establece el area de clipping
          procedure SetClippingAreaAll;
          procedure FillRect(x1, y1, x2, y2:integer; c:LongWord );            //Rectangulo relleno
          procedure FillRectClip(x1, y1, x2, y2:integer; c:LongWord );
          procedure RectangleClip(x1, y1, x2, y2:integer; c:LongWord );
          procedure Rectangle(x1, y1, x2, y2:integer; c:LongWord);
          {lines}
          procedure Line(x1, y1, x2, y2 :integer; c:LongWord);
          procedure LineProc(x1, y1, x2, y2 :integer; c:LongWord; PutPixelProc:TPutPixelProc);
          procedure LineBold(x1, y1, x2, y2 :integer; c:LongWord; thickNess:integer);
          procedure LineSoft(x1, y1, x2, y2 :integer; c:LongWord);
          {scroll}
          procedure ScrollDown( lines:integer );                              //le hace scroll down a la imagen completa
          procedure ScrollUp( lines:integer );                                //le hace scroll down a la imagen completa
          procedure BitsSet(x,y:integer; c:LongWord; bitmask:LongWord);       //mmm... bitsset...
          procedure TileXY(var x,y:integer);                                  //calcula la coordenada mosaico de una cordenada común x y
          function  MemUsed:integer;                                          //estimado de la memoria usada por la imagen

          {transference}
          procedure PutImageClip(x,y:integer;  Source:TDpx);overload;          //lo convencional
          procedure PutImage(x,y:integer; Source:TDpx);overload;
          procedure GetImageClip(x,y:integer; Source:TDpx);overload;
          procedure GetImage(x,y:integer; Source:TDpx);overload;

          {alphablending}
          procedure PutPixelAlpha(x, y:integer; c:longWord);{alpha 8 MSBs }   //pixel con alpha blending
          procedure PutPixelAlphaClip(x, y:integer; c:longWord);
          procedure PutPixelAlphaTile(x, y:integer; c:longWord);
          function  GetPixelAntiAlpha(x, y:integer; cfondo:LongWord):LongWord; //mmmm... ni te lo imaginas, la operacion contraria a PutPixelAlpha.. WOW!!
          procedure RemoveMatteBackground( cfondo: LongWord );                //applica AntiAlpha a toda la imagen
          procedure SetAlpha(x,y:integer; alpha:byte);                         //establece alpha,no afecta el color
          procedure SetColor(x,y:integer; c:LongWord);                         //establece color,no afecta el alpha
          procedure CopyAlphaChannel( Source:TDpx );                           //copia el canal alpha de un lao pa otro, OJO, IMAGEN DEL MISMO SIZE
          procedure BuildAlphaFromIntensity;                                   //genera alpha de la propia intesidad de color de la imagen
          procedure BuildAlphaFromIntensityOf( Source:TDpx );                  //genera alpha de la intesidad de color de otra imagen, OJO, IMAGEN DEL MISMO SIZE
          procedure BuildAlphaFromRedChannelOf( Source:TDpx );                 //genera alpha de intesidad del canal rojo de otra imagen, (util para imagenes mascara blaco y negro)
          procedure PutImageTransp(x,y:integer; const alpha:byte; Source:TDpx);       //imagen con un alpha total
          procedure PutImageApha(x,y:integer; Source:TDpx);                   //imagen con alpha por pixel
          procedure PutImageAlphaClip(x,y:integer;  Source:TDpx; alpha: byte);
          procedure PutImageTranspClip(x,y:integer; Source:TDpx);
          {Alphablending Optimized!! 3.5 times FASTER!}
          procedure PreOptimizeAlpha(MinAlpha :byte=10; MaxAlpha :byte=250);     {recomendado 10 - 250} //Recalcula las lineas transparentes y opacas de la imagen para el !!!
          function  CreateOptSprite(MinAlpha :byte=10; MaxAlpha :byte=250): TOptSprite;
          procedure PutImageAlphaOpt( x,y:integer; Source:TDpx);overload;              //  el Source es el que debe estar optimizado offcourse
          procedure PutImageAlphaOpt(x,y:integer; Source:TOptSprite);overload;
          procedure PutImageAlphaClipOpt( x,y:integer; Source:TDpx);overload;          //..
          procedure PutImageAlphaClipOpt( x,y:integer; Source:TOptSprite);overload;
          {shading and color}
          procedure DarkPutPixel(x, y:integer; darkness:byte);                 //pixel obscuro... mmm... zátanico
          procedure PutPixelClipDark(x, y:integer; darkness:byte);             // y así...
          procedure PutImageDarkMap(x,y:integer; Source:TDpx);
          procedure PutImageDarkMapClip(x, y: integer; Source: TDpx);
          procedure Dark(x1, y1, x2, y2:integer; darkness:byte);overload;
          procedure ColorDarkClip(x1, y1, x2, y2:integer; darkness:byte);overload;
          procedure Dark( darkness :byte);overload;
          procedure AlhpaDec( decr :byte );//not implemented
          procedure ColorTint( x, y :integer; tintColor:LongWord);             //reemplaza el color-tinte de un pixel, preserva intensidad
          procedure ColorTintClip( x, y :integer; tintColor:LongWord);
          {Mask!}
          procedure UpdateMaskCoords( keyColor:LongWord);                      //calcula lineas opacas dado un KeyColor > bkColor
          procedure PutImageMasked(x,y:integer; source:TDpx);overload;        //imagen con mascara precalculada
          procedure PutImageClipMasked(x,y:integer; source:TDpx);overload;    //..
          procedure PutImageMasked(x,y:integer; source:TDpx; const MaskCoords:TPixelSegments; Relx, Rely:integer);overload; //lo mismo mas flexible
          {Interpolation}
          function GetPixelInterpLineal(const  x,y :single ):LongWord;        //a partir de coordenadas reales interpola los colores para coger un pixel
          function GetPixelInterpClip( x,y :single ):LongWord;          //...
          {file loading}
          procedure LoadFromTGA( FileName:string  );overload;                 //carga un TGA, con RLE o no, pero solo de 32bits de fichero en disco
          procedure LoadFromTGA( FileData:Pointer );overload;                 //carga un TGA, con RLE o no, pero solo de 32bits de memoria
          {filters}
          {properties}
          property Height:integer read FHeight;
          property Width:integer read FWidth;
          property AlphaOptimized:boolean read fAlphaOptimized;
          property ImgBoundingRect:TRct read fImgBoundingRect;
      end;

    TOptSprite = class
    public
      constructor create;
      destructor destroy;override;
    private
       fWidth, fHeight :integer;
       fMyMemSize :integer;
    protected
       fAlphaTransparentCoords :TPixelSegments;                    //posiciones lineas con alpha blending generadas por ReCalculateAlphaMaskCoords
       fAlphaOpaqueCoords      :TPixelSegments;
       fTransparentPixels      :array of array of LongWord;
       fOpaquePixels           :array of array of LongWord;
       fImgBoundingRect        :TRct;
    public
      property Width:integer read fWidth;
      property Height:integer read fHeight;
      property MemUsed:integer read fMyMemSize;
      property ImgBoundingRect:TRct read fImgBoundingRect;
    end;

    TDpxRef = class(TDpx)
    public
      constructor Create;overload;
      constructor Create(iniWidth, iniHeight:integer);overload;
      destructor destroy;override;
    private
    protected
      fRefImage :TDpx;
      fMyPosX,
      fMyPosY   :integer;
    public
      procedure UpdateScanLines;override;
      procedure InitSize( aWidth, aHeight:integer);
      procedure GrabTo( img :TDpx;  x,y :integer );               //WARNING! si las dimensiones sobrepasan la ImageReferenciada... CRASH!!!
      procedure SetPos( x, y: integer );
      property PosX:integer read fMyPosX;
      property PosY:integer read fMyPosY;
      property RefImage:TDpx read fRefImage;
    end;

    TDpxMem = class(TDpx)
    public
      constructor create;
      destructor destroy;override;
    private
    protected
      fBuffer :PDWordArray;
      fBufferSize :integer;
    public
      procedure UpdateScanLines;override;
      procedure ReSize(aWidth, aHeight:integer);override;
    end;

     //funcioncitas útiles e interezantes...
     function ARGB(a,r,g,b:byte):LongWord;
     function InterColor(c1,c2:TRGBA; Tendencia:single=0.5):TRGBA;{tendecia de 0.0 a 1.0} //color intermedio entre dos colores dada una tendencia
     function AverageColors(Colors:array of LongWord):TRGBA;      //promedia colores
     function difference(c1,c2:TRGBA):integer;overload;                    //cuan diferentes son dos colores, Result=0 identicos, mientras mayor Result, mas diferentes
     function difference(c1,c2:LongWord):integer;overload;

     //rectángulos y puntos
     function PtInRct(const Rct: TRct; const P: TPt): Boolean;overload;
     function PtInRct(const Rct: TRct; x,y:integer): Boolean;overload;
     function Pt(x,y:integer):TPt;

implementation




function Pt(x,y:integer):TPt;inline;
begin
  Result.x :=x;
  Result.y :=y;

end;

function PtInRct(const Rct: TRct; const P: TPt): Boolean;
begin
  Result := (P.X >= Rct.x1) and (P.X < Rct.x2) and (P.Y >= Rct.y1)
    and (P.Y < Rct.y2);
end;

function PtInRct(const Rct: TRct; x,y:integer): Boolean;overload;
begin
  Result := (X >= Rct.x1) and (X < Rct.x2) and (Y >= Rct.y1)
    and (Y < Rct.y2);
end;

function difference(c1,c2:TRGBA):integer;inline;
begin
  Result:=
   abs(c2.b - c1.b) +
   abs(c2.g - c1.g) +
   abs(c2.r - c1.r) +
   abs(c2.a - c1.a);
end;

function difference(c1,c2:LongWord):integer;inline;
begin
  Result:=
   abs(TRGBA(c2).b - TRGBA(c1).b) +
   abs(TRGBA(c2).g - TRGBA(c1).g) +
   abs(TRGBA(c2).r - TRGBA(c1).r) +
   abs(TRGBA(c2).a - TRGBA(c1).a);
end;


function AverageColors(Colors:array of LongWord):TRGBA;
var
  sumR, sumG, sumB, sumA :integer;
  c1   :TRGBA;
  cant,i  :integer;
begin
  sumR:=0; sumG:=0; sumB:=0; sumA:=0;
  for i:=Low(Colors) to High(Colors) do
  begin
    c1.color:=Colors[i];
    sumR:=sumR + c1.r;
    sumG:=sumG + c1.g;
    sumB:=sumB + c1.b;
    sumA:=sumA + c1.a;
  end;
  cant:=Length(Colors);
  Result.r:= sumR div cant;
  Result.g:= sumG div cant;
  Result.a:= sumA div cant;
  Result.b:= sumB div cant;
end;

function InterColor(c1,c2:TRGBA; Tendencia:single):TRGBA;inline;
begin
  Result.a:=Round(c1.a + (c2.a-c1.a)*Tendencia);
  Result.r:=Round(c1.r + (c2.r-c1.r)*Tendencia);
  Result.g:=Round(c1.g + (c2.g-c1.g)*Tendencia);
  Result.b:=Round(c1.b + (c2.b-c1.b)*Tendencia);
end;


{Function GetAValue(ARGB:LongWord):byte;
begin
  Result:=Byte(ARGB shr 24);
end;
}
function ARGB;assembler;
asm
  {AL --> a}
  {DL --> r}
  {CL --> g}
  {pila-> b}   {result EAX}
  SHL   EAX,8
  OR    AL,B
  SHL   EAX,8
  OR    AL,CL
  SHL   EAX,8
  OR    AL,DL;
end;

procedure TDpx.AlhpaDec(decr: byte);
begin

end;

procedure TDpx.PutImageApha(x, y: integer; Source: TDpx);
var
  i,j:integer;
  src,dst:PRGBA;
begin
  for j:=0 to Source.Height-1 do
  begin
    src := @ ( Source.FScanlines[j] )^[0];
    dst  := @ ( FScanlines[y+j] )^[x];
    for i:=0 to Source.Width-1 do
    begin
        //este código debo repetirlo porque delhi todabía no tiene inline :)
        //
        dst.r:=dst.r + ( ( src.a * ( src.r - dst.r + 1 ) ) shr 8);
        dst.g:=dst.g + ( ( src.a * ( src.g - dst.g + 1 ) ) shr 8);
        dst.b:=dst.b + ( ( src.a * ( src.b - dst.b + 1 ) ) shr 8);
        if (dst.a+ src.a)>255 then dst.a:=255 else dst.a:=dst.a + src.a;
        //
        inc(src);
        inc(dst);
    end;
  end;
end;

procedure TDpx.PutImageTransp(x, y:integer; const alpha:byte; Source: TDpx);
var
  i,j:integer;
  src, dst :PRGBA;
begin

  for j:=0 to Source.Height-1 do
  begin
    src := @ ( Source.FScanlines[j] )^[0];
    dst  := @ ( FScanlines[y+j] )^[x];
    for i:=0 to Source.Width-1 do
    begin
        //este código debo repetirlo porque delhi todabía no tiene inline :)
        //
        dst.r:=dst.r + ( ( alpha * ( src.r - dst.r + 1 ) ) shr 8);
        dst.g:=dst.g + ( ( alpha * ( src.g - dst.g + 1 ) ) shr 8);
        dst.b:=dst.b + ( ( alpha * ( src.b - dst.b + 1 ) ) shr 8);
        if (dst.a+ alpha)>255 then dst.a:=255 else dst.a:=dst.a + alpha;
        //
        inc(src);
        inc(dst);
    end;
  end;

end;


procedure TDpx.PutPixelAlpha(x, y: integer; c: longWord);
var
  dst:PRGBA;
  src:TRGBA;
begin
    src.color := c;
    dst := @( (FScanLines[y])[x]);
    //este código debo repetirlo porque delhi todabía no tiene inline :)
    //
    dst.r:=dst.r + ( ( src.a * ( src.r - dst.r + 1 ) ) shr 8);
    dst.g:=dst.g + ( ( src.a * ( src.g - dst.g + 1 ) ) shr 8);
    dst.b:=dst.b + ( ( src.a * ( src.b - dst.b + 1 ) ) shr 8);
    if (dst.a+ src.a)>255 then dst.a:=255 else dst.a:=dst.a + src.a;
end;


procedure TDpx.SetAlpha(x, y: integer; alpha: byte);
begin
  PRGBA(  ( FScanlines[y] )^[x] ).a := alpha
end;

function TDpx.GetPixelAntiAlpha(x, y: integer;   cfondo: LongWord): LongWord;
var
  n:TRGBA;
  rn,gn,bn :byte;
  rf,gf,bf :byte;
  rc,gc,bc:integer;
  alpha:byte;
begin
  n:=TRGBA(getpixel(x,y));
  alpha:=n.a;
  rn:=n.r;
  gn:=n.g;
  bn:=n.b;
  rf:=TRGBA(cfondo).r;
  gf:=TRGBA(cfondo).g;
  bf:=TRGBA(cfondo).b;
  if alpha<>0 then
  begin
    rc:=rf+((rn-rf)*255) div alpha;
    gc:=gf+((gn-gf)*255) div alpha;
    bc:=bf+((bn-bf)*255) div alpha;
    if (rc< 0)  then rc:=0;
    if (gc< 0)  then gc:=0;
    if (bc< 0)  then bc:=0;

  end else
  begin
    rc:=0;
    gc:=0;
    bc:=0;
  end;
  Result:=ARGB(alpha,rc,gc,bc);
end;



procedure TDpx.PutPixelClipDark(x, y: integer; darkness: byte);
begin
  if (x>=FX1Clip) and (y>=FY1Clip) and   (x<=FX2Clip) and (y<=FY2Clip)
  then DarkPutPixel(x,y,darkness);
end;

function TDpx.GetPixelClip(x, y: integer): LongWord;
begin
  if (x>=FX1Clip) and (y>=FY1Clip) and   (x<=FX2Clip) and (y<=FY2Clip)
  then Result:= ( FScanLines[y] )^[x]
  else Result:=0;
end;

procedure TDpx.PutPixelClip(x, y: integer; c: LongWord);
begin
  if (x>=FX1Clip) and (y>=FY1Clip) and   (x<=FX2Clip) and (y<FY2Clip)
     then  ( FScanlines[y] )^[x] := c ;
end;

procedure TDpx.CLS(c : longWord);
begin
  FillRect(0,0, FWidth-1, FHeight-1, c);
end;

procedure TDpx.Dark(x1, y1, x2, y2: integer; darkness: byte);
 var
  PDWArr :PDWordArray;
  cont   :integer;
  F    :LongWord;
  r,g,b,a  :byte;
begin
  while y1<=y2 do
  begin
    PDWArr:=FScanLines[y1];
    cont:=x1;
    While cont<=x2 do
    begin
      F:=PDWArr[cont];
      r:=Byte(F);
      g:=Byte(F shr 8);
      b:=Byte(F shr 16);
      a:=0;
      r:=(r*DarkNess)div 256;
      g:=(g*DarkNess)div 256;
      b:=(b*DarkNess)div 256;
      PDWArr[cont]:=ARGB(a,r,g,b);
      inc(cont);
    end;
    inc(y1);
  end;

end;

procedure TDpx.Dark(darkness: byte);
begin
  Dark(0,0, FWidth-1, FHeight-1, darkness);
end;

procedure TDpx.PutImageDarkMap(x, y: integer; Source: TDpx);
var
  i,j:integer;
begin
  for j:=0 to Source.Height-1 do
    for i:=0 to Source.Width-1 do
    begin
      //TODO: Manual inline here ^_^ and optmize
      DarkPutPixel(x+i, y+j, Source.GetPixel(i,j)  );
    end;
end;

procedure TDpx.PutImageDarkMapClip(x, y: integer; Source: TDpx);
var
  i,j:integer;
  xcut1,xcut2,
  ycut1,ycut2 :integer;
begin
  xLineCutting(x,x+Source.Width-1, xcut1, xcut2);
  yLineCutting(y,y+Source.Height-1, ycut1, ycut2);
 { si se pinta toito toito}
{  if (xcut1=0) and (xcut2=0) and (ycut1=0) and (ycut2=0) then
  begin
    DarkMapPutImage(x,y, source);
    exit;
  end;}

  {ver si no se pinta naita}
  if (xcut1 > (Source.Width-1)) or
     (xcut2 > (Source.Width-1)) or
     (ycut1 > (Source.Height-1)) or
     (ycut2 > (source.Height-1)) then exit;

  for j:=ycut1 to (Source.Height-1-ycut2) do
    for i:=xcut1 to (Source.Width-1-xcut2) do
    begin
      DarkPutPixel(x+i, y+j,  Byte(Source.GetPixel(i,j))  );
    end;
end;


procedure TDpx.DarkPutPixel(x, y: integer; darkness: byte);
var
  F:LongWord;
  r,g,b:byte;
begin
  F:= ( FScanlines[y] )^[x];
  r:=Byte(F);
  g:=Byte(F shr 8);
  b:=Byte(F shr 16);
  r:=(r*DarkNess)div 256;
  g:=(g*DarkNess)div 256;
  b:=(b*DarkNess)div 256;
   ( FScanlines[y] )^[x]:=( r or (g shl 8) or (b shl 16) );
end;

destructor TDpx.destroy;
begin
  inherited ;
end;


function TDpx.GetPixel(x,y:integer): LongWord;
begin
  result:=  ( FScanlines[y] )^[x] ;
end;

procedure TDpx.PutImageClip(x, y: integer; Source: TDpx);
var
  xcut1,xcut2,
  ycut1,ycut2 :integer;
  j :integer;
  size :integer;
begin
  xLineCutting(x,x+Source.Width-1, xcut1, xcut2);
  yLineCutting(y,y+Source.Height-1, ycut1, ycut2);

   { si se pinta toito toito}
{  if (xcut1=0) and (xcut2=0) and (ycut1=0) and (ycut2=0) then
  begin
    PutImage(x,y, source);
    exit;
  end; }


  {ver si no se pinta naita}
  if (xcut1 > (Source.Width-1)) or
     (xcut2 > (Source.Width-1)) or
     (ycut1 > (Source.Height-1)) or
     (ycut2 > (source.Height-1)) then exit;

  size:=(source.width-xcut2-xcut1)*4;
  for j:=y+ycut1 to y+source.height-1-ycut2 do
  begin
    move(
           ( Source.FScanLines[j-y] )^[0+xcut1],
           ( FScanLines[j] )^[x+xcut1], size
        );
  end;
end;

procedure TDpx.PutPixel( x, y: integer; c: LongWord);
begin
   ( FScanlines[y] )^[x] := c ;
end;

procedure TDpx.PutImage(x, y: integer; Source: TDpx);
var
  j :integer;
  size :integer;
begin
  size:=source.width*4;
  for j:=y to y+source.height-1 do
  begin
    move(
           ( Source.FScanLines[j-y] )^[0],
           ( FScanLines[j] )^[x], size
        );
  end;
end;

procedure TDpx.UpdateMaskCoords(keyColor: LongWord);
begin
  bkColorTmp:=keyColor;
  fMaskCoords:=GetLineCoords(PixelTestGMC);
end;

procedure TDpx.PutImageMasked(x, y: integer; source: TDpx);
var
  segcont,i :integer;
  seginf    :TPixelSegm;
begin
  segcont:=Length(source.fMaskCoords);
  for i:=0 to segcont-1 do
  begin
      seginf:=source.fMaskCoords[i];
      move(
           ( Source.FScanLines[ SegInf.y ] )^[SegInf.x],
           ( FScanLines[SegInf.y+y] )^[SegInf.x+x],
          SegInf.cantx4
        );
  end;
end;

procedure TDpx.SetColor(x, y: integer; c: LongWord);
var
  gc:LongWord;
begin
  gc:=  ( FScanlines[y] )^[x] ;
   ( FScanlines[y] )^[x] := (gc and $ff000000) or (c and $00ffffff);

end;

{Afecta el pixel solo en los bits que son 1 en la mascara}
procedure TDpx.BitsSet(x, y: integer; c, bitmask: LongWord);
var
  gc:LongWord;
begin
  gc:=  ( FScanlines[y] )^[x] ;
   ( FScanlines[y] )^[x] := (gc and (not bitmask)) or (c and bitmask);
end;

function TDpx.GetPixelInterpLineal(const x, y: single): LongWord;
var
  c1,c2,cA,cB,cR   :TRGBA;
  x1, y1   :integer;
  x2,y2 :integer;
  tendx,
  tendy   :single;{de 1 a 0}
begin
  x1:=trunc(x);
  x2:=x1+1;
  y1:=trunc(y);
  y2:=y1+1;
  tendx:=x-x1; if tendx<0 then tendx:=1+tendx;
  tendy:=y-y1; if tendx<0 then tendx:=1+tendx;

  c1.color:= ( FScanLines[y1] )^[x1] ;
  c2.color:= ( FScanLines[y1] )^[x2] ;
  cA:=InterColor(c1,c2,Tendx);

  c1.color:= ( FScanLines[y2] )^[x1] ;
  c2.color:= ( FScanLines[y2] )^[x2] ;
  cB:=InterColor(c1,c2,Tendx);

  cR:=InterColor(cA,cB,tendy);
  Result:=cR.color;
end;



function TDpx.GetPixelInterpClip(x, y: single): LongWord;
var
  c1,c2,cA,cB,cR   :TRGBA;
  x1,x2,
  y1,y2   :integer;
  tendx,
  tendy   :single;{de 1 a 0}
begin
 if (x>=FX1Clip) and (y>=FY1Clip) and   (x<FX2Clip) and (y<FY2Clip) then
 begin

  x1:=trunc(x);
  x2:=x1+1;
  y1:=trunc(y);
  y2:=y1+1;
  tendx:=x-x1; if tendx<0 then tendx:=1+tendx;
  tendy:=y-y1; if tendx<0 then tendx:=1+tendx;

  //then Result:= ( FScanLines[y] )^[x]
  //  else Result:=0;

  c1.color:= ( FScanLines[y1] )^[x1] ;
  c2.color:= ( FScanLines[y1] )^[x2] ;

  cA:=InterColor(c1,c2,Tendx);
  c1.color:= ( FScanLines[y2] )^[x1] ;
  c2.color:= ( FScanLines[y2] )^[x2] ;
  cB:=InterColor(c1,c2,Tendx);
  cR:=InterColor(cA,cB,tendy);
  Result:=cR.color;

  end else Result:=0;
end;

procedure TDpx.ScrollDown(lines: integer);
var
 a,i    :integer;
 size :integer;
begin
  a:=fHeight-1;   // a donde va la linea i es de donde viene
  size:=fWidth*4; //cantidad de bytes a mover
  for i:=fHeight-lines-1 downto 0 do
  begin
    move( FScanLines[i]^, FScanLines[a]^, size);
    dec(a);
  end;
end;

procedure TDpx.GetImage(x, y: integer; Source: TDpx);
var
  j :integer;
  size :integer;
  miy :integer;
begin
  size:=fWidth*4;
  miy:=0;
  for j:=(y) to (y)+fheight-1 do
  begin
    move(
           ( Source.FScanLines[j] )^[x],
           ( FScanLines[miy] )^[0], size
        );
    inc(miy);
  end;
end;

constructor TDpx.Create;
begin
  fAlphaOptimized := false;
end;

procedure TDpx.GetImageClip(x, y: integer; Source: TDpx);
var
  j :integer;
  size :integer;
  miy :integer;
  xcut1,xcut2,
  ycut1,ycut2 :integer;
begin
  Source.xLineCutting(x,x+Width-1, xcut1, xcut2);
  Source.yLineCutting(y,y+Height-1, ycut1, ycut2);

  { si se coje toito toito}
 { if (xcut1=0) and (xcut2=0) and (ycut1=0) and (ycut2=0) then
  begin
    GetImage(x,y, source);
    exit;
  end;}

  {ver si no se coje naita}
  if (xcut1 > (Width-1)) or
     (xcut2 > (Width-1)) or
     (ycut1 > (Height-1)) or
     (ycut2 > (Height-1)) then exit;

  size:=(width-xcut1-xcut2)*4;
  miy:=0;
  for j:=y+ycut1 to y+height-1-ycut2 do
  begin
    move(
           ( Source.FScanLines[j] )^[x+xcut1],
           ( FScanLines[miy+ycut1] )^[0+xcut1], size
        );
    inc(miy);
  end;
end;

procedure TDpx.PutPixelAlphaClip(x, y: integer; c: longWord);
var
  src,
  dst :PRGBA;
begin
  if (x>=FX1Clip) then
    if (x<=FX2Clip) then
      if (y>=FY1Clip) then
        if (y<=FY2Clip) then
 begin
    src := @c;
    dst := @( (FScanLines[y])[x]);
    //este código debo repetirlo porque delhi todabía no tiene inline :)
    //
    dst.r:=dst.r + ( ( src.a * ( src.r - dst.r + 1 ) ) shr 8);
    dst.g:=dst.g + ( ( src.a * ( src.g - dst.g + 1 ) ) shr 8);
    dst.b:=dst.b + ( ( src.a * ( src.b - dst.b + 1 ) ) shr 8);
    if (dst.a+ src.a)>255 then dst.a:=255 else dst.a:=dst.a + src.a;
 end;
end;

procedure TDpx.PutImageAlphaClip(x, y: integer;  Source: TDpx; alpha: byte);
var
  i,j:integer;
  xcut1,xcut2,
  ycut1,ycut2 :integer;
  src,dst:PRGBA;
begin
  xLineCutting(x,x+Source.Width-1, xcut1, xcut2);
  yLineCutting(y,y+Source.Height-1, ycut1, ycut2);
  { si se pinta toito toito}
{  if (xcut1=0) and (xcut2=0) and (ycut1=0) and (ycut2=0) then
  begin
    AlphaPutImage(x,y, alpha, Source);
    exit;
  end;}


  {ver si no se pinta naita}
  if (xcut1 > (Source.Width-1)) or
     (xcut2 > (Source.Width-1)) or
     (ycut1 > (Source.Height-1)) or
     (ycut2 > (source.Height-1)) then exit;


  //pinto recortao
  for j:=ycut1 to (Source.Height-1-ycut2) do
  begin
    src := @ ( Source.FScanlines[j] )^[xcut1];
    dst  := @ ( FScanlines[y+j] )^[x+xcut1];
    for i:=xcut1 to (Source.Width-1-xcut2) do
    begin
        //este código debo repetirlo porque delhi todabía no tiene inline :)
        //
        dst.r:=dst.r + ( ( alpha * ( src.r - dst.r + 1 ) ) shr 8);
        dst.g:=dst.g + ( ( alpha * ( src.g - dst.g + 1 ) ) shr 8);
        dst.b:=dst.b + ( ( alpha * ( src.b - dst.b + 1 ) ) shr 8);
        if (dst.a+ alpha)>255 then dst.a:=255 else dst.a:=dst.a + alpha;
        //
        inc(src);
        inc(dst);
    end;
  end;

end;

procedure TDpx.PutImageTranspClip(x, y: integer; Source: TDpx);
var
  i,j:integer;
  xcut1,xcut2,
  ycut1,ycut2 :integer;
  src,dst:PRGBA;
begin
  xLineCutting(x,x+Source.Width-1, xcut1, xcut2);
  yLineCutting(y,y+Source.Height-1, ycut1, ycut2);

  { si se pinta toito toito}
{  if (xcut1=0) and (xcut2=0) and (ycut1=0) and (ycut2=0) then
  begin
    PutAlphaImage(x,y, Source);
    exit;
  end;   }

  {ver si no se pinta naita}
  if (xcut1 > (Source.Width-1)) or
     (xcut2 > (Source.Width-1)) or
     (ycut1 > (Source.Height-1)) or
     (ycut2 > (source.Height-1)) then exit;

  for j:=ycut1 to (Source.Height-1-ycut2) do
  begin
    src := @ ( Source.FScanlines[j] )^[xcut1];
    dst  := @ ( FScanlines[y+j] )^[x+xcut1];
    for i:=xcut1 to (Source.Width-1-xcut2) do
    begin
        //este código debo repetirlo porque delhi todabía no tiene inline :)
        //
        dst.r:=dst.r + ( ( src.a * ( src.r - dst.r + 1 ) ) shr 8);
        dst.g:=dst.g + ( ( src.a * ( src.g - dst.g + 1 ) ) shr 8);
        dst.b:=dst.b + ( ( src.a * ( src.b - dst.b + 1 ) ) shr 8);
        if (dst.a+ src.a)>255 then dst.a:=255 else dst.a:=dst.a + src.a;
        //
        inc(src);
        inc(dst);
    end;
  end;

end;

procedure TDpx.ScrollUp(lines: integer);
var
 a,i,size    :integer;
begin
  a:=0;   // a donde va la linea i es de donde viene
  size:=fWidth*4; //cantidad de dwords a mover
  for i:=lines to fHeight-1 do
  begin
    move( FScanLines[i]^, FScanLines[a]^, size);
    inc(a);
  end;
end;




procedure TDpx.PutImageMasked(x, y: integer; source: TDpx;
  const MaskCoords: TPixelSegments; Relx, Rely: integer);
var
  segcont,i :integer;
  seginf    :TPixelSegm;
begin
  segcont:=Length(MaskCoords);
  for i:=0 to segcont-1 do
  begin
      seginf:=MaskCoords[i];
      move(
           ( Source.FScanLines[ SegInf.y+Rely ] )^[SegInf.x+Relx],
           ( FScanLines[SegInf.y+y] )^[SegInf.x+x],
          SegInf.cantx4
        );
  end;
end;

function TDpx.GetLineCoords(condition: TPixelTestFunc): TPixelSegments;
var
  segcont :integer;
  x,y     :integer;
  c       :longword;
  segsize :integer;
  xini    :integer;
begin
  segcont := 0;
  xini:=0;
  for y:=0 to fHeight-1 do
  begin
    segsize:=0;
    for x:=0 to fWidth-1 do
    begin
      c:=GetPixel(x,y);
      if condition(c) then
      begin
        inc(segsize);
        if segsize=1 then xini:=x;
      end else
      begin
        if segsize>0 then
          begin
            //guarda este
            inc(segcont);
            setlength(result,segcont);
            Result[SegCont-1].x:=xini;
            Result[SegCont-1].y:=y;
            Result[SegCont-1].cantx4:=(segsize)*4;
          end;
        segsize:=0;
      end;
    end;//for x

    //se acaba la fila y hay pixeles en segsize
    if segsize>0 then
    begin
    //guarda este
      inc(segcont);
      setlength(result,segcont);
      Result[SegCont-1].x:=xini;
      Result[SegCont-1].y:=y;
      Result[SegCont-1].cantx4:=(segsize)*4;
    end;

  end; //for y

end;


{  PreOptimizeAlpha }

{
Esta procedimineto no necesita ni pretende ser Rádpido, es para llamar una sola vez.

Como optimiza?

  ignora todos los pixeles con alpha menores de MinAlpha
  guarda Dos Arreglos
  Uno - Los píxeles transparentes:
    Con las coordenadas y cantidad de píxeles continuos en linea horizontal que tengan alpha entre MinAlpha y MaxAlpha
  Dos - Los píxeles opacos
    Con las coordenadas y cantidad de píxeles continuos en linea horizontal que tengan alpha mayor que 250;

}
//TODO: Se puede hallar también la nueva area reducida, o bounding rect,
//Los algoritmos de clipping pueden beneficiarse con esto
//
procedure TDpx.PreOptimizeAlpha(MinAlpha, MaxAlpha: byte);  {recomendado 10 - 250}
var
  R1, R2:TRct;
begin
  MinAlphaTmp:=MinAlpha;
  MaxAlphaTmp:=MaxAlpha;

  fAlphaTransparentCoords:=GetLineCoords( PixelTestTransparent );
  fAlphaOpaqueCoords:=GetLineCoords( PixelTestOpaque );

  //Bounding Rect;
  R1 := CalcBoundingRect(fAlphaTransparentCoords);
  R2 := CalcBoundingRect(fAlphaOpaqueCoords);
  if R2.x1 < R1.x1 then R1.x1 := R2.x1;
  if R2.x2 > R1.x2 then R1.x2 := R2.x2;
  if R2.y1 < R1.y1 then R1.y1 := R2.y1;
  if R2.y2 > R1.y2 then R1.y2 := R2.y2;

  fImgBoundingRect := R1;

  fAlphaOptimized := true;
end;

function TDpx.PixelTestGMC(c: LongWord): boolean;
begin
  Result:=c<>bkColorTmp;
end;

function TDpx.PixelTestOpaque(c: LongWord): boolean;
var
  alpha :byte;
begin
  alpha:= TRGBA(c).a;
  Result := alpha >= MaxAlphaTmp;
end;

function TDpx.PixelTestTransparent(c: LongWord): boolean;
var
  alpha :byte;
begin
 alpha:=TRGBA(c).a;
 Result := (alpha >= MinAlphaTmp) and (alpha<MaxalphaTmp);
end;

procedure TDpx.PutImageAlphaOpt(x, y: integer; Source: TDpx);
var
  i,j :integer;
  seginf  :TPixelSegm;
  src,dst :PRGBA;
begin

  for i:=0 to high(Source.fAlphaOpaqueCoords) do
  begin
      seginf:=Source.fAlphaOpaqueCoords[i];
      move(
           ( Source.FScanLines[ SegInf.y ] )^[SegInf.x],
           ( FScanLines[SegInf.y+y] )^[SegInf.x+x],
          SegInf.cantx4
        );

  end;

  for i:=0 to high(Source.fAlphaTransparentCoords) do
  begin
    SegInf:=Source.fAlphaTransparentCoords[i];
    src := @(   ( Source.FScanLines[ SegInf.y ]  )^[SegInf.x + 0]         );
    dst := @(   ( FSCanLines[ SegInf.y+y]        )^[SegInf.x+x+0]         );
    for j:=0 to (SegInf.cantx4 shr 2)-1 do
    begin
      //PutPixelAlpha(SegInf.x+x+j, SegInf.y+y, Source.GetPixel(SegInf.x+j,SegInf.y) );

      dst.r:=dst.r +  ( (src.a * ( src.r - dst.r)) shr 8 );
      dst.g:=dst.g +  ( (src.a * ( src.g - dst.g))  shr 8 );
      dst.b:=dst.b +  ( (src.a * ( src.b - dst.b))  shr 8 );

      if (dst.a+ src.a)>255 then dst.a:=255 else dst.a:=dst.a + src.a;
      inc(src);
      inc(dst);
    end;
  end;
end;


procedure TDpx.FillRectClip(x1, y1, x2, y2: integer; c: LongWord);
var
  xcut1,xcut2,
  ycut1,ycut2 :integer;
  P:PLongWord;
  i,j:integer;
begin
  xLineCutting(x1,x2, xcut1, xcut2);
  yLineCutting(y1,y2, ycut1, ycut2);

  {ver si no se pinta naita}
  if (xcut1 > (x2-x1)) or
     (xcut2 > (x2-x1)) or
     (ycut1 > (y2-y1)) or
     (ycut2 > (y2-y1)) then exit;


    //FillRect(x1+xcut1, y1+ycut1, x2-xcut2, y2-ycut2, c);
    //evitando la llamada
    x1 := x1+xcut1; y1:=y1+ycut1;
    x2 := x2-xcut2; y2:=y2-ycut2;
    for j:=y1 to y2 do
    begin
      P:=PLongWord(FScanLines[j]);
      inc(P, x1);
      for i:=x1 to x2 do
      begin
        P^:=c;
        inc(P);
      end;
    end;


end;

procedure TDpx.xLineCutting(x1, x2: integer; var xCut1,
  xCut2: integer);
begin
  if x1 < FX1Clip then xCut1:= FX1Clip-x1 else xCut1:=0;
  if x2 > FX2Clip then xCut2:= x2-FX2Clip else xCut2:=0;
end;

procedure TDpx.yLineCutting(y1, y2: integer; var yCut1,
  yCut2: integer);
begin
  if y1 < FY1Clip then yCut1:= FY1Clip-y1 else yCut1:=0;
  if y2 > FY2Clip then yCut2:= y2-FY2Clip else yCut2:=0;
end;

procedure TDpx.SetClippingArea(x1, y1, x2, y2: integer);
begin
  FX1Clip:=x1;
  FX2Clip:=x2;
  FY1Clip:=y1;
  FY2Clip:=y2;
end;

procedure TDpx.ColorDarkClip(x1, y1, x2, y2: integer; darkness: byte);
var
  xcut1,xcut2,
  ycut1,ycut2 :integer;
begin
  xLineCutting(x1,x2, xcut1, xcut2);
  yLineCutting(y1,y2, ycut1, ycut2);
 { si se pinta toito toito}
{  if (xcut1=0) and (xcut2=0) and (ycut1=0) and (ycut2=0) then
  begin
    Dark(x1, y1, x2, y2, darkness);
    exit;
  end;}

  {ver si no se pinta naita}
  if (xcut1 > (x2-x1)) or
     (xcut2 > (x2-x1)) or
     (ycut1 > (y2-y1)) or
     (ycut2 > (y2-y1)) then exit;
  Dark(x1+xcut1, y1+ycut1, x2-xcut2, y2-ycut2, darkness);
end;

procedure TDpx.PutImageClipMasked(x, y: integer; source: TDpx);
var
  segcont,i :integer;
  seginf    :TPixelSegm;
  xcut1, xcut2  :integer;
  ycut1, ycut2  :integer;
  cant  :integer;
begin
  xLineCutting(x,x+Source.Width-1, xcut1, xcut2);
  yLineCutting(y,y+Source.Height-1, ycut1, ycut2);
 { si se pinta toito toito}
{  if (xcut1=0) and (xcut2=0) and (ycut1=0) and (ycut2=0) then
  begin
    PutImageMasked(x,y, source);
    exit;
  end;}

  {ver si no se pinta naita}
  if (xcut1 > (Source.Width-1)) or
     (xcut2 > (Source.Width-1)) or
     (ycut1 > (Source.Height-1)) or
     (ycut2 > (source.Height-1)) then exit;

  {si llega aqui es que hay posible fragmentacion}
  segcont:=Length(source.fMaskCoords);
  for i:=0 to segcont-1 do
  begin
      seginf:=source.fMaskCoords[i];
      if ((seginf.y+y) >= fY1Clip) and ((seginf.y+y) <= fY2Clip) then
      begin
        cant:=seginf.cantx4 div 4;
        xLineCutting( seginf.x+x, seginf.x+x+(cant)-1, xcut1, xcut2);
        if (xcut1<(cant)) and (xcut2<(cant)) then
        move(
           ( Source.FScanLines[ SegInf.y ] )^[SegInf.x+xcut1],
           ( FScanLines[SegInf.y+y] )^[SegInf.x+x+xcut1],
          SegInf.cantx4 - xcut1*4 - xcut2*4
        );
      end;
  end;
end;

//TODO:   Revisar Optimización
//NOTA:
{
esto está enyerbao!! :)
}
procedure TDpx.PutImageAlphaClipOpt(x, y: integer; Source: TDpx);
var
  segcont,i,j :integer;
  seginf    :TPixelSegm;
  xcut1, xcut2  :integer;
  ycut1, ycut2  :integer;
  cant  :integer;
  src,dst :PRGBA;
begin
  //hago el clipping primero al ImgBoundingRect
  with Source.ImgBoundingRect do
  begin
    xLineCutting(x1+x, x2+x, xcut1, xcut2);
    yLineCutting(y1+y, y2+y, ycut1, ycut2);
  end;


  // si no hay clipping se pinta completa y vamo hechando... que no pasa na
  if (xcut1 or xcut2 or ycut1 or ycut2)= 0 then
  begin
    PutImageAlphaOpt(x,y, source);
    exit;
  end;


  //hago el clipping
  xLineCutting(x,x+Source.Width-1, xcut1, xcut2);
  yLineCutting(y,y+Source.Height-1, ycut1, ycut2);


  {ver si no se pinta naita}
  if (xcut1 > (Source.Width-1)) or
     (xcut2 > (Source.Width-1)) or
     (ycut1 > (Source.Height-1)) or
     (ycut2 > (source.Height-1)) then exit;

  {si llega aqui es que hay  fragmentacion}
  //allá voy...
  //primero los segmentos OPACOS

  segcont:=Length(Source.fAlphaOpaqueCoords);
  for i:=0 to segcont-1 do
  begin
    seginf:=Source.fAlphaOpaqueCoords[i];
    //verifico que el segmento esté entre fY1Clip y fY2Clip
    if ((seginf.y+y) >= fY1Clip) and ((seginf.y+y) <= fY2Clip) then
    begin
      //calculo en cant la longitud del segmento
      cant:=seginf.cantx4 div 4;
      //le hago clipping
      xLineCutting( seginf.x+x, seginf.x+x+(cant)-1, xcut1, xcut2);
      //y luego... mmm candela aqui que hice? pero funciona oite
      if (xcut1<(cant)) and (xcut2<(cant)) then
      move(
           ( Source.FScanLines[ SegInf.y ] )^[SegInf.x+xcut1],
           ( FScanLines[SegInf.y+y] )^[SegInf.x+x+xcut1],
          SegInf.cantx4 - xcut1*4 - xcut2*4
        );
    end;
  end;

  //ahroa los que tienen transparencia
  //lo mismo...
  segcont:=Length(Source.fAlphaTransparentCoords);
  for i:=0 to segcont-1 do
  begin
    SegInf:=Source.fAlphaTransparentCoords[i];
    //cliping por las y
    if ((seginf.y+y) >= fY1Clip) and ((seginf.y+y) <= fY2Clip) then
    begin
      cant:=seginf.cantx4 div 4;
      //clipng por las x/
      xLineCutting( seginf.x+x, seginf.x+x+(cant)-1, xcut1, xcut2);

      if (xcut1<(cant)) and (xcut2<(cant)) then
      begin
        //segmento empieza en Source(SegInf.x+xcut1, SegInf.y)
        //destino empieza e Dest(SegInf.x+x+xcut1, SegInf.y+y);
        src := @(   ( Source.FScanLines[ SegInf.y ]  )^[SegInf.x + xcut1]         );
        dst := @(   ( FSCanLines[ SegInf.y+y]        )^[SegInf.x + x + xcut1]         );

        for j:=0+xcut1 to (SegInf.cantx4 div 4)-1-xcut2 do
        begin
          //PutPixelAlpha(SegInf.x+x+j, SegInf.y+y, Source.GetPixel(SegInf.x+j,SegInf.y) );
          dst.r:=dst.r + ( ( src.a * ( src.r - dst.r + 1 ) ) shr 8);
          dst.g:=dst.g + ( ( src.a * ( src.g - dst.g + 1 ) ) shr 8);
          dst.b:=dst.b + ( ( src.a * ( src.b - dst.b + 1 ) ) shr 8);
          if (dst.a+ src.a)>255 then dst.a:=255 else dst.a:=dst.a + src.a;
          inc(src);
          inc(dst);
        end;
      end;
    end;
  end;

end;

{
  Carga TGA,
  Solo para TGA de 32 bits, con o sin comprimir.


}
procedure TDpx.LoadFromTGA(FileName: string);
var
  f :file;
  header  :TTGAHeader;
  x,y     :integer;
  data    :LongWord;

  procedure DeCompress;
  var
    b :byte;
    i :byte;
    cant  :integer;
  begin
    cant:=0;
    Repeat
      BlockRead(f, b, 1);
      if b >= 128 then {bit 7=1}
      begin
        b:=b and 127;                      // pone el bit a 0
        BlockRead(f, data, 4);
        for i:=0 to b do
        begin
          PutPixel(cant mod header.Width, (header.height-1) - (cant div header.Width), data);
          inc(cant);
        end;
      end else         {bit 7=0}
      begin
        for i:=0 to b do
        begin
          BlockRead(f, data, 4);
          PutPixel(cant mod header.Width, (header.Height-1)-(cant div header.Width), data);
          inc(cant);
        end;
      end;
    until Cant >= Header.width* Header.height;
  end;

begin
  assign(f, FileName);
  Reset(f,1);
  BlockRead(f, header, SizeOf(Header));
  {Bitmap.Width:=Header.Width;
  Bitmap.Height:=Header.Height;
  RefreshScanLines;  }
  ReSize(Header.Width, Header.Height);
  case Header.Compression of
  $02:begin {uncompressed}

        for y:=Header.Height-1 downto 0 do
          for x:=0 to Header.Width-1 do
          begin
            BlockRead(f, data, 4);
            PutPixel(x,y, data);
          end;
    end;
  $0A:begin {RLE Run Length Encoding}
        Decompress;
    end;
  end;
  CloseFile(f);
end;


procedure TDpx.ColorTint(x, y: integer; tintColor: LongWord);
var
  bwlevel  :byte;
  res :^TRGBA;
  rs,gs,bs  :integer;
  overflow  :integer;
begin
  {color tint replace}
  //pointing to the pixel to tint
  res :=  @  ( FScanlines[y] )^[x] ;
  //calculating black and white level of source
  bwlevel := (res^.r + res^.g + res^.b) div 3;

  //llevo el source de 0-2 y lo multimplico por el tint.color
  rs := (bwlevel * TRGBA(tintColor).r) div 128;
  gs := (bwlevel * TRGBA(tintColor).g) div 128; //optimized
  bs := (bwlevel * TRGBA(tintColor).b) div 128;

  //the color overflow
  overflow := 0;
  if rs>255 then
  begin
    overflow := overflow + (rs-255);
    rs:=255;
  end;
  if gs>255 then
  begin
    overflow := overflow + (gs-255);
    gs:=255;
  end;
  if bs>255 then
  begin
    overflow := overflow + (bs-255);
    bs:=255;
  end;
  if overflow>0 then
  begin
      //proportionally sum overflow to low color levels
      rs := rs + ((255-rs)*overflow) div 256;
      gs := gs + ((255-gs)*overflow) div 256;
      bs := bs + ((255-bs)*overflow) div 265;
  end;

  //make sure nothing pass 255 again
  rs := rs and $ff;
  gs := gs and $ff;
  bs := bs and $ff;
  res^.r:=rs;
  res^.g:=gs;
  res^.b:=bs;
end;

procedure TDpx.ColorTintClip(x, y: integer; tintColor: LongWord);
begin
  if (x>=FX1Clip) and (y>=FY1Clip) and   (x<=FX2Clip) and (y<=FY2Clip) then
    ColorTint( x, y , tintColor );
end;

procedure TDpx.CopyAlphaChannel(Source: TDpx);
var
  x,y :integer;
begin
  for y:=0 to Height-1 do
    for x:=0 to Width-1 do
    begin
      TRGBA(  (FScanLines[y])[x] ).a := TRGBA(  (Source.FScanLines[y])[x] ).a;
    end;
end;

procedure TDpx.BuildAlphaFromIntensity;
var
  c :TRGBA;
  x,y :integer;
begin
  for y:=0 to height-1 do
    for x:=0 to width-1 do
    begin
       c := TRGBA (  (FScanLines[y])[x] );
       //TODO: They says this is not the correct calc, but whatever..
       c.a := (c.g + c.r + c.b) div 3;
        (FScanLines[y])[x] := c.color;
    end;
end;

procedure TDpx.BuildAlphaFromIntensityOf(Source: TDpx);
var
  c :TRGBA;
  x,y :integer;
begin
  for y:=0 to height-1 do
    for x:=0 to width-1 do
    begin
       c := TRGBA (  (Source.FScanLines[y])[x] );
       TRGBA(  (FScanLines[y])[x] ).a := (c.g + c.r + c.b) div 3;
    end;
end;

procedure TDpx.PutPixelbuffer(x, y: integer; var buffer; pixelsCount: integer);
begin
    move(
          TDWORDArray( buffer )[0],
           ( FScanLines[y] )^[x], pixelsCount shl 2
        );
end;

procedure TDpx.PutPixelTile(x, y: integer; c: LongWord);
begin
  TileXY(x,y);
   ( FScanlines[y] )^[x] := c ;
end;

procedure TDpx.TileXY(var x, y: integer);
begin
  if x>=FX1Clip then x := FX1Clip + (x-FX1Clip) mod (FX2Clip-FX1Clip)
                else x := FX2Clip + (x-FX1Clip) mod (FX2Clip-FX1Clip);
  if y>=FY1Clip then y := FY1Clip + (y-FY1Clip) mod (FY2Clip-FY1Clip)
                else y := FY2Clip + (y-FY1Clip) mod (FY2Clip-FY1Clip);
end;

function TDpx.GetPixelTile(x, y: integer): LongWord;
begin
  TileXY(x,y);
  Result:= ( FScanLines[y] )^[x]
end;

procedure TDpx.PutPixelAlphaTile(x, y: integer; c: longWord);
var
  src,
  dst:PRGBA;
begin
 TileXY(x,y);
 begin
    src := @c;
    dst := @((FScanLines[y])[x]);
    //este código debo repetirlo porque delhi todabía no tiene inline :)
    //
    dst.r:=dst.r + ( ( src.a * ( src.r - dst.r + 1 ) ) shr 8);
    dst.g:=dst.g + ( ( src.a * ( src.g - dst.g + 1 ) ) shr 8);
    dst.b:=dst.b + ( ( src.a * ( src.b - dst.b + 1 ) ) shr 8);
    if (dst.a+ src.a)>255 then dst.a:=255 else dst.a:=dst.a + src.a;
 end;
end;



function TDpx.MemUsed: integer;
begin
  result := FWidth * FHeight * 4 + FHeight;
end;

procedure TDpx.BuildAlphaFromRedChannelOf(Source: TDpx);
var
  x,y :integer;
begin
  for y:=0 to height-1 do
    for x:=0 to width-1 do
       PRGBA( @ (FScanLines[y])[x] ).a := PRGBA( @ (Source.FScanLines[y])[x] ).r;
end;

function TDpx.GetScanLinePtrA(y: integer):PDWordArray  ;
begin
  Result := FScanLines[y];
end;

function TDpx.GetScanLinePtr(y: integer): pointer;
begin
  Result := FScanLines[y];
end;

function TDpx.GetPtr(x, y: integer): pointer;
begin
  result:= @(  ( FScanlines[y] )^[x] ) ;
end;

function TDpx.GetPtrA(x, y: integer):PDWordArray  ;
begin
  result:= @(  ( FScanlines[y] )^[x] ) ;
end;

{ TOptSprite }

constructor TOptSprite.create;
begin
  fMyMemSize :=0;
end;

destructor TOptSprite.destroy;
begin

  inherited;
end;


{
Crea un objeto independiente OptSprite
con segementos opacos y transparentes,
}
function TDpx.CreateOptSprite(MinAlpha :byte=10; MaxAlpha :byte=250): TOptSprite;
var
  tc,
  oc  :integer;
  i :integer;
  p :pointer;
begin
  PreOptimizeAlpha(MinAlpha, MaxAlpha);
  Result := TOptSprite.create;
  tc := Length(fAlphaTransparentCoords);
  oc := Length(fAlphaOpaqueCoords);
  SetLength(Result.fAlphaTransparentCoords, tc);
  SetLength(Result.fAlphaOpaqueCoords, oc);
  move(fAlphaTransparentCoords[0], Result.fAlphaTransparentCoords[0], tc*SizeOf(TPixelSegm) );
  move(fAlphaOpaqueCoords[0], Result.fAlphaOpaqueCoords[0], oc*SizeOf(TPixelSegm));
  Setlength( Result.fTransparentPixels, tc);
  SetLength(Result.fOpaquePixels, oc);
  Result.fMyMemSize := tc*SizeOf(TPixelSegm) + oc*SizeOf(TPixelSegm);
  Result.fImgBoundingRect := fImgBoundingRect;
  for i:=0 to tc-1 do
  begin
    Setlength(Result.fTransparentPixels[i], fAlphaTransparentCoords[i].cantx4 div 4);
    p := GetPtr(fAlphaTransparentcoords[i].x, fAlphaTransparentCoords[i].y);
    move(p^, Result.fTransparentPixels[i][0], fAlphaTransparentCoords[i].cantx4 );
    inc(Result.fMyMemSize, fAlphaTransparentCoords[i].cantx4);
  end;
  for i:=0 to oc-1 do
  begin
    SetLength(Result.fOpaquePixels[i], fAlphaOpaqueCoords[i].cantx4 div 4);
    p:= GetPtr(fAlphaOpaqueCoords[i].x, fAlphaOpaqueCoords[i].y);
    move(p^, Result.fOpaquePixels[i][0], fAlphaOpaqueCoords[i].cantx4);
    inc(Result.fMyMemSize, fAlphaOpaqueCoords[i].cantx4);
  end;
  Result.fWidth := fWidth;
  Result.fHeight := fHeight;
end;

procedure TDpx.PutImageAlphaOpt(x, y: integer; Source: TOptSprite);
var
  i,j :integer;
  seginf    :TPixelSegm;
  src,dst :PRGBA;
begin

  for i:=0 to high(Source.fAlphaOpaqueCoords) do
  begin
      seginf:=Source.fAlphaOpaqueCoords[i];
      move(
          Source.fOpaquePixels[i][0] ,
            ( FScanLines[SegInf.y+y] )^[SegInf.x+x],
          SegInf.cantx4
        );
  end;

  for i:=0 to high(Source.fAlphaTransparentCoords) do
  begin
    SegInf:=Source.fAlphaTransparentCoords[i];
    src := @(  Source.fTransparentPixels[i][0]         );
    dst := @(  ( FSCanLines[ SegInf.y+y]        )^[SegInf.x+x+0]         );
    for j:=0 to (SegInf.cantx4 div 4)-1 do
    begin
      //PutPixelAlpha(SegInf.x+x+j, SegInf.y+y, Source.GetPixel(SegInf.x+j,SegInf.y) );
      dst.r:=dst.r + ( ( src.a * ( src.r - dst.r ) ) shr 8);
      dst.g:=dst.g + ( ( src.a * ( src.g - dst.g ) ) shr 8);
      dst.b:=dst.b + ( ( src.a * ( src.b - dst.b ) ) shr 8);

      if (dst.a+ src.a)>255 then dst.a:=255 else dst.a:=dst.a + src.a;
      inc(src);
      inc(dst);
    end;
  end;
end;

procedure TDpx.PutImageAlphaClipOpt(x, y: integer; Source: TOptSprite);
var
  segcont,i,j :integer;
  seginf    :TPixelSegm;
  xcut1, xcut2  :integer;
  ycut1, ycut2  :integer;
  cant  :integer;
  src,dst :PRGBA;
begin
  //hago el clipping primero al ImgBoundingRect
  with Source.ImgBoundingRect do
  begin
    xLineCutting(x1+x, x2+x, xcut1, xcut2);
    yLineCutting(y1+y, y2+y, ycut1, ycut2);
  end;

  // si no hay clipping se pinta completa y vamo hechando... que no pasa na
  if (xcut1 or xcut2 or ycut1 or ycut2)= 0 then
  begin
    PutImageAlphaOpt(x,y, source);
    exit;
  end;

  //hago el clipping
  xLineCutting(x,x+Source.Width-1, xcut1, xcut2);
  yLineCutting(y,y+Source.Height-1, ycut1, ycut2);


  {ver si no se pinta naita}
  if (xcut1 > (Source.Width-1)) or
     (xcut2 > (Source.Width-1)) or
     (ycut1 > (Source.Height-1)) or
     (ycut2 > (source.Height-1)) then exit;

  {si llega aqui es que hay  fragmentacion}
  //allá voy...
  //primero los segmentos OPACOS

  segcont:=Length(Source.fAlphaOpaqueCoords);
  for i:=0 to segcont-1 do
  begin
    seginf:=Source.fAlphaOpaqueCoords[i];
    //verifico que el segmento esté entre fY1Clip y fY2Clip
    if ((seginf.y+y) >= fY1Clip) and ((seginf.y+y) <= fY2Clip) then
    begin
      //calculo en cant la longitud del segmento
      cant:=seginf.cantx4 div 4;
      //le hago clipping
      xLineCutting( seginf.x+x, seginf.x+x+(cant)-1, xcut1, xcut2);
      //y luego... mmm candela aqui que hice? pero funciona oite
      if (xcut1<(cant)) and (xcut2<(cant)) then
      move(
          // ( Source.FScanLines[ SegInf.y ] )^[SegInf.x+xcut1],
          Source.fOpaquePixels[i][xcut1],
           ( FScanLines[SegInf.y+y] )^[SegInf.x+x+xcut1],
          SegInf.cantx4 - xcut1*4 - xcut2*4
        );
    end;
  end;       

  //ahroa los que tienen transparencia
  //lo mismo...
  segcont:=Length(Source.fAlphaTransparentCoords);
  for i:=0 to segcont-1 do
  begin
    SegInf:=Source.fAlphaTransparentCoords[i];
    //cliping por las y
    if ((seginf.y+y) >= fY1Clip) and ((seginf.y+y) <= fY2Clip) then
    begin
      cant:=seginf.cantx4 div 4;
      //clipng por las x/
      xLineCutting( seginf.x+x, seginf.x+x+(cant)-1, xcut1, xcut2);

      if (xcut1<(cant)) and (xcut2<(cant)) then
      begin
        //segmento empieza en Source(SegInf.x+xcut1, SegInf.y)
        //destino empieza e Dest(SegInf.x+x+xcut1, SegInf.y+y);
        //src := @(   ( Source.FScanLines[ SegInf.y ]  )^[SegInf.x + xcut1]         );
        src := @( Source.fTransparentPixels[i][xcut1] );
        dst := @(   ( FSCanLines[ SegInf.y+y]        )^[SegInf.x + x + xcut1]         );

        for j:=0+xcut1 to (SegInf.cantx4 div 4)-1-xcut2 do
        begin
          //PutPixelAlpha(SegInf.x+x+j, SegInf.y+y, Source.GetPixel(SegInf.x+j,SegInf.y) );
          dst.r:=dst.r + ( ( src.a * ( src.r - dst.r + 1 ) ) shr 8);
          dst.g:=dst.g + ( ( src.a * ( src.g - dst.g + 1 ) ) shr 8);
          dst.b:=dst.b + ( ( src.a * ( src.b - dst.b + 1 ) ) shr 8);
          if (dst.a+ src.a)>255 then dst.a:=255 else dst.a:=dst.a + src.a;
          inc(src);
          inc(dst);
        end;
      end; //if xcut1
    end;//if seginf

  end;//for i
end;

{ TDpxRef }

constructor TDpxRef.create;
begin
  Inherited create;
  fWidth := 10;
  fHeight := 10;
end;

constructor TDpxRef.Create(iniWidth, iniHeight: integer);
begin
  fWidth := iniWidth;
  FHeight := iniHeight;
  SetClippingArea(0,0, fWidth, fHeight);
end;

destructor TDpxRef.destroy;
begin

  inherited;
end;

procedure TDpxRef.GrabTo(img: TDpx; x, y: integer);
begin
  fRefImage := img;
  fMyPosX := x;
  fMyPosY := y;
  UpdateScanLines;
end;

procedure TDpxRef.InitSize(aWidth, aHeight: integer);
begin
  fWidth := aWidth;
  fHeight := aHeight;
  SetClippingArea(0,0, aWidth-1, aHeight-1);  
end;

procedure TDpxRef.SetPos(x, y: integer);
begin
  fMyPosX := x;
  fMyPosY := y;
  UpdateScanLines;
end;

procedure TDpxRef.UpdateScanLines;
var
  j :integer;
begin
  if fHeight<>Length(FScanLines) then SetLength(FScanLines, fHeight);
  for j:=0 to fHeight-1 do
    FScanLines[j] := @( fRefImage.FScanLines[fMyPosY + j][fMyPosX] );
end;

procedure TDpx.ReSize(aWidth, aHeight: integer);
begin
  fWidth := aWidth;
  fHeight := aHeight;
  SetClippingArea(0,0, aWidth-1, aHeight-1);
  UpdateScanlines;
end;

{ TDpxBuf }

constructor TDpxMem.create;
begin
  fWidth := 8;
  fHeight := 8;
    SetClippingArea(0,0, fWidth-1, fHeight-1);
  fBufferSize := fWidth*fHeight*4;
  GetMem(fBuffer, fBufferSize);
  UpdateScanLines;
end;

destructor TDpxMem.destroy;
begin
  freeMem(fBuffer, fBufferSize);
  inherited;
end;

procedure TDpxMem.ReSize;
begin
  freeMem(fBuffer, fBufferSize);
  fWidth := aWidth;
  fHeight := aHeight;
  SetClippingArea(0,0, fWidth-1, fHeight-1);
  fBufferSize := fWidth*fHeight*4;
  GetMem(fBuffer, fBufferSize);
  UpdateScanLines;
end;

procedure TDpxMem.UpdateScanLines;
var
  j:integer;
begin
  if fHeight<>Length(FScanLines) then SetLength(FScanLines, fHeight);
  for j:=0 to fHeight-1 do
    FScanLines[j] := @( fBuffer[ j * fWidth ] );
end;

procedure TDpx.SetClippingAreaAll;
begin
  SetClippingArea(0,0, fWidth-1, fHeight-1);
end;

procedure TDpx.FillRect(x1, y1, x2, y2: integer; c: LongWord);
var
  P :PLongWord;
  i,j :integer;
begin
  //Este código lo hize en ensamblador y según las pruebas
  //resultó ser mas lento mi código ASM que el codigo optimizado que
  //genera el compilador de delphi!! wow!
  //aunque.. quien dijo que yo le meto tanto al ASM
  //Antes usaba While ahora con for y inc(pointer) aumentó la
  //velócidad en un 20%
  for j:=y1 to y2 do
  begin
    P:=PLongWord(FScanLines[j]);
    inc(P, x1);
    for i:=x1 to x2 do
    begin
      P^:=c;
      inc(P);
    end;
  end;
end;

procedure TDpx.Rectangle(x1, y1, x2, y2: integer; c: LongWord);
var
  P :PLongWord;
  i :integer;
begin
  //linea horizontal de arriba
  P := PLongWord(FScanLines[y1]);
  inc(P, x1);
  for i := x1 to x2 do
  begin
    P^ := c;
    inc(P);
  end;
  //linea horizontal de abajo
  P := PLongWord(FScanLines[y2]);
  inc(P, x1);
  for i := x1 to x2 do
  begin
    p^ := c;
    inc(P);
  end;
  //linea vertical 1
  for i:=(y1+1) to (y2-1) do
    FSCanLines[i][x1]:=c;
  //linea vertical 2
  for i:=(y1+1) to (y2-1) do
    FSCanLines[i][x2]:=c;
end;

procedure TDpx.RectangleClip(x1, y1, x2, y2: integer; c: LongWord);
var
  xcut1,xcut2,
  ycut1,ycut2 :integer;
  P:PLongWord;
  i:integer;
begin
  //new
  {
    Arreglado aumentó velocidad en 200%
  }
 // x2:=x2+1;
 // y2:=y2+1;
  xLineCutting(x1,x2, xcut1, xcut2);
  yLineCutting(y1,y2, ycut1, ycut2);
  //si no quedó totalmente afuera...
  if (xcut1<(x2-x1)) and (xcut2<(x2-x1)) and (ycut1<(y2-y1)) and (ycut2<(y2-y1)) then
  begin
    x1 := x1 + xcut1;
    x2 := x2 - xcut2;
     //linea horizontal de arriba
    if (ycut1=0)  then
    begin
      P := PLongWord(FScanLines[y1]);
      inc(P, x1);
      for i := x1 to x2 do
      begin
        P^ := c;
        inc(P);
      end;
    end;
    //linea horizontal de abajo
    if (ycut2=0)  then
    begin
      P := PLongWord(FScanLines[y2]);
      inc(P, x1);
      for i := x1 to x2 do
      begin
        p^ := c;
        inc(P);
      end;
    end;
  //linea vertical 1

    y1 := y1 + ycut1;
    y2 := y2 - ycut2;
    if xcut1=0  then
      for i:=(y1) to (y2) do
        FSCanLines[i][x1]:=c;
    //linea vertical 2
    if xcut2=0 then
      for i:=(y1) to (y2) do
        FSCanLines[i][x2]:=c;

  end;//if
end;


procedure TDpx.RemoveMatteBackground(cfondo: LongWord);
var
  i,j:integer;
  pureColor :LongWord;
begin
  for i := 0 to fWidth-1 do
    for j := 0 to fHeight-1 do
    begin
      pureColor := GetPixelAntiAlpha(i,j,cfondo);
      SetColor(i,j,pureColor);
    end;
end;

procedure TDpx.Line(x1, y1, x2, y2: integer; c: LongWord);
Var
  LgDelta, ShDelta, LgStep, ShStep, Cycle: Integer;
  t:integer;
begin
  {
  delay - x100000
  140ms
  }
  LgDelta := X2 - X1;
  ShDelta := Y2 - Y1;
  if LgDelta < 0 then
    begin
      LgDelta := -LgDelta;
      LgStep := -1;
    end
  else
    LgStep := 1;
  if ShDelta < 0 then
    begin
      ShDelta := -ShDelta;
      ShStep := -1;
    end
  else
    ShStep := 1;
  if LgDelta > ShDelta then
    begin
      Cycle := LgDelta shr 1; { LgDelta / 2 }
      While X1 <> X2 do
      begin
        { PutPixel(X1, Y1, Color); }
        //px-mod
        FScanLines[y1][x1] := c;
        //
        Inc(X1, LgStep);
        Inc(Cycle, ShDelta);
        if Cycle > LgDelta then
        begin
          Inc(Y1, ShStep);
          Dec(Cycle, LgDelta);
        end;
      end;
    end
  else
    begin
      Cycle := ShDelta shr 1; { ShDelta / 2 }
      //xchg(LgDelta, ShDelta);
      t := LgDelta;
      LgDelta := ShDelta;
      ShDelta := t;
      //xchg(LgStep, ShStep);
      t := LgStep;
      LgStep := ShStep;
      ShStep := t;
      While Y1 <> Y2 do
      begin
        FScanLines[y1][x1] := c; { PutPixel(X1, Y1, Color); }
        Inc(Y1, LgStep);
        Inc(Cycle, ShDelta);
        if Cycle > LgDelta then
        begin
          Inc(X1, ShStep);
          Dec(Cycle, LgDelta);
        end;
      end;
    end;
end;

procedure TDpx.LineProc(x1, y1, x2, y2: integer; c: LongWord;
  PutPixelProc: TPutPixelProc);
Var
  LgDelta, ShDelta, LgStep, ShStep, Cycle: Integer;
  t:integer;
begin
  LgDelta := X2 - X1;
  ShDelta := Y2 - Y1;
  if LgDelta < 0 then
    begin
      LgDelta := -LgDelta;
      LgStep := -1;
    end
  else
    LgStep := 1;
  if ShDelta < 0 then
    begin
      ShDelta := -ShDelta;
      ShStep := -1;
    end
  else
    ShStep := 1;
  if LgDelta > ShDelta then
    begin
      Cycle := LgDelta shr 1; { LgDelta / 2 }
      While X1 <> X2 do
      begin
        { PutPixel(X1, Y1, Color); }
        //px-mod
        PutPixelProc(x1,y1, c);
        //
        Inc(X1, LgStep);
        Inc(Cycle, ShDelta);
        if Cycle > LgDelta then
        begin
          Inc(Y1, ShStep);
          Dec(Cycle, LgDelta);
        end;
      end;
    end
  else
    begin
      Cycle := ShDelta shr 1; { ShDelta / 2 }
      //xchg(LgDelta, ShDelta);
      t := LgDelta;
      LgDelta := ShDelta;
      ShDelta := t;
      //xchg(LgStep, ShStep);
      t := LgStep;
      LgStep := ShStep;
      ShStep := t;
      While Y1 <> Y2 do
      begin
        PutPixelProc(x1, y1, c); { PutPixel(X1, Y1, Color); }
        Inc(Y1, LgStep);
        Inc(Cycle, ShDelta);
        if Cycle > LgDelta then
        begin
          Inc(X1, ShStep);
          Dec(Cycle, LgDelta);
        end;
      end;
   end;

end;

procedure TDpx.LineBold(x1, y1, x2, y2: integer; c: LongWord;
  thickNess: integer);
Var
  LgDelta, ShDelta, LgStep, ShStep, Cycle: Integer;
  t,i:integer;
begin
  thickness:=thickness-1;
  LgDelta := X2 - X1;
  ShDelta := Y2 - Y1;
  if LgDelta < 0 then
    begin
      LgDelta := -LgDelta;
      LgStep := -1;
    end
  else
    LgStep := 1;
  if ShDelta < 0 then
    begin
      ShDelta := -ShDelta;
      ShStep := -1;
    end
  else
    ShStep := 1;
  if LgDelta > ShDelta then
    begin
      Cycle := LgDelta shr 1; { LgDelta / 2 }
      While X1 <> X2 do
      begin
        { PutPixel(X1, Y1, Color); }
        //px-mod
        for i:=0-(thickness div 2) to (thickness-(thickness div 2)) do
        begin
          FScanLines[y1+i][x1] := c;
        end;
        //
        Inc(X1, LgStep);
        Inc(Cycle, ShDelta);
        if Cycle > LgDelta then
        begin
          Inc(Y1, ShStep);
          Dec(Cycle, LgDelta);
        end;
      end;
    end
  else
    begin
      Cycle := ShDelta shr 1; { ShDelta / 2 }
      //xchg(LgDelta, ShDelta);
      t := LgDelta;
      LgDelta := ShDelta;
      ShDelta := t;
      //xchg(LgStep, ShStep);
      t := LgStep;
      LgStep := ShStep;
      ShStep := t;
      While Y1 <> Y2 do
      begin
        { PutPixel(X1, Y1, Color); }
        for i:=0-(thickness div 2) to (thickness-(thickness div 2)) do
        begin
          FScanLines[y1][x1+i] := c;
        end;
        Inc(Y1, LgStep);
        Inc(Cycle, ShDelta);
        if Cycle > LgDelta then
        begin
          Inc(X1, ShStep);
          Dec(Cycle, LgDelta);
        end;
      end;
    end;
end;

procedure TDpx.LineSoft(x1, y1, x2, y2: integer; c: LongWord);
{
Aquí hize unos inventos para meter antialiasing... y funciona goood!
}
Var
  LgDelta, ShDelta, LgStep, ShStep, Cycle: Integer;
  t:integer;
  alpha :LongWord;
  segCount :integer;
  i:integer;
Const
  MAXALPHA = 255;
begin

  LgDelta := X2 - X1;
  ShDelta := Y2 - Y1;
  if LgDelta < 0 then
    begin
      LgDelta := -LgDelta;
      LgStep := -1;
    end
  else
    LgStep := 1;
  if ShDelta < 0 then
    begin
      ShDelta := -ShDelta;
      ShStep := -1;
    end
  else
    ShStep := 1;
  if LgDelta > ShDelta then
    begin
      Cycle := LgDelta shr 1; { LgDelta / 2 }
      SegCount :=0;
      While X1 <> X2 do
      begin
        { PutPixel(X1, Y1, Color); }
        FScanLines[y1][x1] := c;
        inc(SegCount);
        //
        Inc(X1, LgStep);
        Inc(Cycle, ShDelta);
        if Cycle > LgDelta then
        begin
          //smooth stuff
          for i:=0 to segCount-1 do
          begin
            Alpha:=((i+1)*MAXALPHA) div (SegCount+1);
            if ShStep<0 then Alpha := MAXALPHA-alpha;
            Alpha := Alpha Shl 24;
            c := (c and $ffFFff) or Alpha;
            PutPixelAlpha(x1-LgStep-i*lgStep, y1-1, c);
            Alpha:=MAXALPHA-(((i+1)*MAXALPHA) div (SegCount+1));
            if ShStep<0 then Alpha := MAXALPHA-alpha;
            Alpha:=Alpha shl 24;
            c := (c and $ffFFff) or Alpha;
            PutPixelAlpha(x1-LgStep-i*LgStep, y1+1, c);
          end;
          SegCount := 0;
          //
          Inc(Y1, ShStep);
          Dec(Cycle, LgDelta);
        end;
      end;//while
      //si queda algo en segcount//
      for i:=0 to segCount-1 do
      begin
        Alpha:=((i+1)*MAXALPHA) div (SegCount+1);
        if ShStep<0 then Alpha := MAXALPHA-alpha;
        Alpha := Alpha Shl 24;
        c := (c and $ffFFff) or Alpha;
        PutPixelAlpha(x1-LgStep-i*lgStep, y1-1, c);
        Alpha:=MAXALPHA-(((i+1)*MAXALPHA) div (SegCount+1));
        if ShStep<0 then Alpha := MAXALPHA-alpha;
        Alpha:=Alpha shl 24;
        c := (c and $ffFFff) or Alpha;
        PutPixelAlpha(x1-LgStep-i*LgStep, y1+1, c);
      end;
    end
  else
    begin
      Cycle := ShDelta shr 1; { ShDelta / 2 }
      //xchg(LgDelta, ShDelta);
      t := LgDelta;
      LgDelta := ShDelta;
      ShDelta := t;
      //xchg(LgStep, ShStep);
      t := LgStep;
      LgStep := ShStep;
      ShStep := t;
      SegCount := 0;
      While Y1 <> Y2 do
      begin
        FScanLines[y1][x1] := c; { PutPixel(X1, Y1, Color); }
        inc(SegCount);
        Inc(Y1, LgStep);
        Inc(Cycle, ShDelta);
        if Cycle > LgDelta then
        begin
          for i:=0 to segCount-1 do
          begin
            Alpha:=MAXALPHA-(((i+1)*MAXALPHA) div (SegCount+1));
            if ShStep<0 then Alpha := MAXALPHA-alpha;
            Alpha := Alpha Shl 24;
            c := (c and $ffFFff) or Alpha;
            PutPixelAlpha(x1+1,y1-LgStep-i*lgStep, c);
            Alpha:=((i+1)*MAXALPHA) div (SegCount+1);
            if ShStep<0 then Alpha := MAXALPHA-alpha;
            Alpha:=Alpha shl 24;
            c := (c and $ffFFff) or Alpha;
            PutPixel(x1-1,y1-LgStep-i*LgStep, c);
          end;
          SegCount := 0;
          Inc(X1, ShStep);
          Dec(Cycle, LgDelta);
        end;
      end;//while
      //por si quedó algo para SegCount
      for i:=0 to segCount-1 do
      begin
        Alpha:=MAXALPHA-(((i+1)*MAXALPHA) div (SegCount+1));
        if ShStep<0 then Alpha := MAXALPHA-alpha;
        Alpha := Alpha Shl 24;
        c := (c and $ffFFff) or Alpha;
        PutPixelAlpha(x1+1,y1-LgStep-i*lgStep, c);
        Alpha:=((i+1)*MAXALPHA) div (SegCount+1);
        if ShStep<0 then Alpha := MAXALPHA-alpha;
        Alpha:=Alpha shl 24;
        c := (c and $ffFFff) or Alpha;
        PutPixelAlpha(x1-1,y1-LgStep-i*LgStep, c);
      end;
    end;
end;

procedure TDpx.LoadFromTGA(FileData: Pointer);
var
  f :PByte;
  header  :TTGAHeader;
  x,y     :integer;
  data    :LongWord;

  procedure DeCompress;
  var
    b :byte;
    i :byte;
    cant  :integer;
  begin
    cant:=0;
    Repeat
      //BlockRead(f, b, 1);
      b := f^;
      inc(f);
      if b >= 128 then {bit 7=1}
      begin
        b:=b and 127;                      // pone el bit a 0
        //BlockRead(f, data, 4);
        Move(f^, data, 4);
        inc(f,4);
        for i:=0 to b do
        begin
          PutPixel(cant mod header.Width, (header.height-1) - (cant div header.Width), data);
          inc(cant);
        end;
      end else         {bit 7=0}
      begin
        for i:=0 to b do
        begin
          //BlockRead(f, data, 4);
          move(f^, data, 4);
          inc(f,4);
          PutPixel(cant mod header.Width, (header.Height-1)-(cant div header.Width), data);
          inc(cant);
        end;
      end;
    until Cant >= Header.width* Header.height;
  end;

begin
  //assign(f, FileName);
  f := FileData;
  //Reset(f,1);
  Move(f^, header, SizeOf(Header));
  inc(f, SizeOf(Header));
  ReSize(Header.Width, Header.Height);
  case Header.Compression of
  $02:begin {uncompressed}

        for y:=Header.Height-1 downto 0 do
          for x:=0 to Header.Width-1 do
          begin
            //BlockRead(f, data, 4);
            Move(f^, data, 4);
            inc(f, 4);
            PutPixel(x,y, data);
          end;
    end;
  $0A:begin {RLE Run Length Encoding}
        Decompress;
    end;
  end;
  //CloseFile(f);
end;



{
 Calcula el Rectángulo mínimo que puede
 abarcar todos los segmentos de píxeles
}
function TDpx.CalcBoundingRect(ca: TPixelSegments): TRct;
var
  i:integer;
begin
  Result.x1 := high(integer); // x mínima
  Result.x2 := low(integer);  // x máxima
  Result.y1 := high(integer); // y mínima
  Result.y2 := low(integer);  // y máxima
  for i:=0 to high(ca) do
  with ca[i] do
  begin
    if x < Result.x1 then Result.x1 := x;
    if y < Result.y1 then Result.y1 := y;
    if (x + cantx4 div 4) > Result.x2 then Result.x2 := (x + cantx4 div 4);
    if (y > Result.y2) then REsult.y2 := y;
  end;
end;

end.
