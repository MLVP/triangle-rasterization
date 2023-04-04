#if sizeof(integer)=8
'#include once "windows.bi"
#endif
#Include once "win/gdiplus.bi"
#include once "file.bi"
#include once "crt.bi"
Using Gdiplus

type IMAGE
	as ubyte ptr dt=0
	as long w, h
	declare sub create(W as long, H as long)
	declare sub destroy()
	
	
	declare function load(filepath as string) as long
	declare function load_stream(DataPtr as any ptr, DataLen as long) as long
	
	declare function save(filepath as string, JPGQual as ULong = 80) as long
	
	
	declare sub flipH()
	declare sub flt_BW()
	declare sub flt_BWRad(r as long)
end type

Sub SFRESIZE (ByVal pSrc As Long PTR, ByVal pDest As Long PTR, ByVal WS As Long, ByVal Hs As Long, ByVal WD As Long, ByVal HD As Long)
  Dim As Long xs = (WS / WD ) * 65536
  Dim As Long ys = (Hs / HD) * 65536
  Dim As Long X, Y, sY
  Dim As Long PTR ps = pSrc
  Dim As Long PTR pt = pDest
  For ty As Long = 0 To HD - 1
	Dim As Long PTR Src = ps + (sY Shr 16) * WS
	For tx As Long = 0 To WD - 1
		* pt = Src[X Shr 16]
		pt += 1
		X += xs
	Next
	sY += ys
	X = 0
  Next
End Sub


sub IMAGE.create(W as long, H as long)
	if this.dt then deallocate(this.dt)
	this.dt = allocate(w*h*4)
	this.w = w
	this.h = h
end sub

sub IMAGE.destroy()
	if this.dt then deallocate(this.dt): this.dt=0
	this.w = 0
	this.h = 0
end sub



function IMAGE.save(filepath as string, JPGQual as ULong = 80) as long

   Dim GDIPlusStartupInput As GDIPLUSSTARTUPINPUT
   Dim As ULONG_PTR GDIPlusToken
   GDIPlusStartupInput.GdiplusVersion = 1   
   If (GdiplusStartup(@GDIPlusToken, @GDIPlusStartupInput, NULL) <> 0) Then Return 0
   Dim as Uinteger  x, y, RowOffset
   Dim As Any Ptr hBitmap
   Dim As BitmapData tBitmapData
   Dim As Rect tRect = Type(0, 0, this.w - 1, this.h - 1)
   GdipCreateBitmapFromScan0(this.w, this.h, this.w*4, PixelFormat32bppARGB, byval this.dt, @hBitmap)
'   GdipCreateBitmapFromScan0(this.w, this.h, 0, PixelFormat32bppARGB, 0, @hBitmap)
'   GdipBitmapLockBits(hBitmap, Cast(Any Ptr, @tRect), ImageLockModeWrite, PixelFormat32bppARGB, @tBitmapData)
'	memcpy  tBitmapData.Scan0, this.dt, this.w*this.h*4
'   GdipBitmapUnlockBits(hBitmap, @tBitmapData)
   
   Dim as Byte iErr = 0

   Dim as UInteger count, size
   GdipGetImageEncodersSize(@count, @size)
   Dim As ImageCodecInfo Ptr pImageCodecInfo, Clsid
   pImageCodecInfo = Allocate(size)
   GdipGetImageEncoders(count, size, pImageCodecInfo)
   
   For i as ulong = 0 to count
      If *cast(wstring ptr, pImageCodecInfo[i].MimeType) = "image/bmp" and Right(filepath, 4) = ".bmp" Then
         If (GdipSaveImageToFile(hBitmap, WStr(filepath), @pImageCodecInfo[i].Clsid, NULL) <> 0) Then iErr += 1
		 
      Elseif *cast(wstring ptr, pImageCodecInfo[i].MimeType) = "image/jpeg" and (Right(filepath, 4) = ".jpg" or Right(filepath, 5) = ".jpeg") Then
         JPGQual = IIf(JPGQual < 0, 0, IIf(JPGQual > 100, 100, JPGQual))
         Dim tParams As EncoderParameters
         Dim EncoderQuality As String = "{1D5BE4B5-FA4A-452D-9CDD-5DB35105E7EB}"
         tParams.Count = 1
         CLSIDFromString(Wstr(EncoderQuality), @tParams.Parameter(0).GUID)
         With tParams.Parameter(0)
            .NumberOfValues = 1
            .Type = EncoderParameterValueTypeLong
            .Value = VarPtr(JPGQual)
         End With
         If GdipSaveImageToFile(hBitmap, WStr(filepath), @pImageCodecInfo[i].Clsid, @tParams) <> 0 Then iErr += 1     
		 
      ElseIf *cast(wstring ptr, pImageCodecInfo[i].MimeType) = "image/gif" and Right(filepath, 4) = ".gif" Then
        If (GdipSaveImageToFile(hBitmap, WStr(filepath), @pImageCodecInfo[i].Clsid, NULL) <> 0) Then iErr += 1
		
      ElseIf *cast(wstring ptr, pImageCodecInfo[i].MimeType) = "image/tiff" and (Right(filepath, 4) = ".tif" or Right(filepath, 5) = ".tiff") Then
        If (GdipSaveImageToFile(hBitmap, WStr(filepath), @pImageCodecInfo[i].Clsid, NULL) <> 0) Then iErr += 1
		
      ElseIf *cast(wstring ptr, pImageCodecInfo[i].MimeType) = "image/png" and Right(filepath, 4) = ".png" Then
        If (GdipSaveImageToFile(hBitmap, WStr(filepath), @pImageCodecInfo[i].Clsid, NULL) <> 0) Then iErr += 1   
		
      Else
         iErr += 1
      End if
   Next

   Deallocate(pImageCodecInfo)

   GdipDisposeImage(hBitmap)
   GdiplusShutdown(GDIPlusToken)
   If iErr Then Return 0

   Return 1
	
end function

function IMAGE.load(filepath as string) as long
	Dim As uinteger TMP
	Dim as any Ptr Img
	Dim As Single w,h
	GDIPLUS.GdiplusStartup(@TMP,@type<GDIPLUS.GDIPLUSSTARTUPINPUT>(1),0)
	if GDIPLUS.GdipLoadImageFromFile(filepath,@Img)>0 then return 1
	GDIPLUS.GdipGetImageDimension(Img,@w,@h)
	if w*h=0 then return 2
	Dim As GDIPLUS.BitmapData Pdata
	Dim As Rect R=Type(0,0,w-1,h-1)
	GDIPLUS.GdipBitmapLockBits(Img,Cast(Any Ptr,@R), GDIPLUS.ImageLockModeRead, PixelFormat32bppARGB, @Pdata)
	if w*h>0 then
		'if this.dt then deallocate(this.dt): this.dt=0
		
		
		if this.dt then
			this.dt = reallocate(this.dt, w*h*4)
		else
			this.dt = allocate(w*h*4)
		end if
		
		memcpy this.dt, cast(ubyte ptr, Pdata.Scan0),w*h*4
		this.w = w
		this.h = h
		GDIPLUS.GdipBitmapUnlockBits( Img, @Pdata)
		GDIPLUS.GdipDisposeImage(Img)
		GDIPLUS.GdiplusShutdown TMP
		return 0 ' OK
	else
		if this.dt then deallocate(this.dt): this.dt=0
		this.w = 0
		this.h = 0
		GDIPLUS.GdipDisposeImage(Img)
		GDIPLUS.GdiplusShutdown TMP
		return 3
	end if
	
end function

function IMAGE.load_stream(DataPtr as any ptr, DataLen as long) as long
	Dim As uinteger TMP
	Dim as any Ptr Img '  GDIPLUS.GpImage
	DIM memStream As ISTREAM PTR
	Dim As HGLOBAL m_hBuffer
	Dim As UByte Ptr pBuffer
	Dim As Single w,h
	dim as long ret
	GDIPLUS.GdiplusStartup(@TMP, @type<GDIPLUS.GDIPLUSSTARTUPINPUT>(1), 0)
	m_hBuffer  = GlobalAlloc( GMEM_MOVEABLE, DataLen )
	if m_hBuffer then
		pBuffer = GlobalLock( m_hBuffer )
		if pBuffer then
			memcpy pBuffer, DataPtr, DataLen
			if CreateStreamOnHGlobal(m_hBuffer, 0, @memStream) = 0 then
				if GDIPLUS.GdipLoadImageFromStream(memStream, @Img) = 0 then
					IUnknown_Release(memStream)
					GDIPLUS.GdipGetImageDimension(Img,@w,@h)
					if w*h>0 then
						Dim As GDIPLUS.BitmapData Pdata
						Dim As Rect R=Type(0,0,w-1,h-1)
						GDIPLUS.GdipBitmapLockBits(Img, Cast(Any Ptr,@R), GDIPLUS.ImageLockModeRead, PixelFormat32bppARGB, @Pdata)
							if this.dt then
								this.dt = reallocate(this.dt, w*h*4)
							else
								this.dt = allocate(w*h*4)
							end if
							memcpy this.dt, cast(ubyte ptr, Pdata.Scan0), w*h*4
							this.w = w
							this.h = h
						GDIPLUS.GdipBitmapUnlockBits( Img, @Pdata)
						ret = 0
					else
						if this.dt then deallocate(this.dt): this.dt=0
						this.w = 0
						this.h = 0
						ret = 5
					end if
					GDIPLUS.GdipDisposeImage(Img)
					GDIPLUS.GdiplusShutdown TMP
				else
					ret = 4
				end if
			else
				ret = 3
			end if
			GlobalUnlock( m_hBuffer )
		else
			ret = 2
		end if
		GlobalFree( m_hBuffer )
	else
		ret = 1
	end if
	return ret
end function




sub IMAGE.flipH()
	dim as long ptr i = cast(long ptr, this.dt)
	
	for y as long = 0 to this.h - 1
		dim t as ulong		
		for x as long = 0 to this.w/2-1
			t = *(i+x)
			*(i + x) = *(i + (w-x-1))
			*(i + (w-x-1)) = t			
		next		
		i += this.w
	next
end sub


sub IMAGE.flt_BW()
	dim as long ac=0
	for n as long = 0 to this.w * this.h - 1
		ac += this.dt[n*4+0] + this.dt[n*4+1]+ this.dt[n*4+1] + this.dt[n*4+2]
	next
	ac= ac / (this.w * this.h*4)
	
	dim as long v
	for n as long = 0 to this.w * this.h - 1
		v = (clng(this.dt[n*4+0]) + this.dt[n*4+1]+ this.dt[n*4+1] + this.dt[n*4+2]) shr 2
		if v> ac then
			this.dt[n*4+0]=255
			this.dt[n*4+1]=255
			this.dt[n*4+2]=255
		else
			this.dt[n*4+0]=0
			this.dt[n*4+1]=0
			this.dt[n*4+2]=0
		end if
	next
end sub

sub IMAGE.flt_BWRad(r as long)
	dim as ubyte ptr tm = allocate(this.w*this.h*4)
	memcpy(tm, this.dt, (this.w*this.h*4))
	
	dim as long j = 0
	dim as long x1,y1,x2,y2
	for y as long = 0 to this.h-1
		for x as long = 0 to this.w-1
			x1= x - r
			y1= y - r
			x2= x + r
			y2= y + r
			if x1<0 then x1=0
			if y1<0 then y1=0
			if x2>this.w-1 then x2=this.w-1
			if y2>this.h-1 then y2=this.h-1
			
			dim as long c=0, i
			for b as long = y1 to y2
			for a as long = x1 to x2
				i = (a+b*this.w)*4
				c += tm[i+0] + tm[i+1]+ tm[i+1] + tm[i+2]
			next
			next
			c = c / ((x2-x1+1)*(y2-y1+1))
			
			dim as long v = clng(tm[j+0]) + tm[j+1]+ tm[j+1] + tm[j+2]	
			if v>c then
				this.dt[j+0] = (255 + tm[j+0])/2
				this.dt[j+1] = (255 + tm[j+1])/2
				this.dt[j+2] = (255 + tm[j+2])/2
			else
				this.dt[j+0] = (0 + tm[j+0])/2
				this.dt[j+1] = (0 + tm[j+1])/2
				this.dt[j+2] = (0 + tm[j+2])/2
			end if
			j+=4
		next
	next
	
	deallocate(tm)
end sub



'screenres 640,480,32

'dim im as IMAGE
''? im.load("test_in.jpg")

'dim as long ff = freefile
'open "test_in.jpg" for binary as #ff
'	dim as long ln=LOF(ff)
'	dim as ubyte ptr dt = allocate(ln)
'	get #ff,1,*dt,ln
'close #ff
'? ln
'? clng(dt)
'? im.load_stream("", dt, ln)

'dim as long i=0
'for y as long =0 to im.h-1
'	for x as long = 0 to im.w-1
'		pset(x,y), im.dt[i]
'		i+=4
'	next
'next

'sleep


