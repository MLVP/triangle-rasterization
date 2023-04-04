Type vec2
    x As single
    y As single
	
	declare function lensqr() as single
	declare function len() as single
	declare function add(v As vec2) As vec2
	declare function subt(v As vec2) As vec2
	declare function mul(s As single) As vec2
	declare function dot(v as vec2) as single
	declare function cross(v as vec2) as single
	declare function norm() As vec2
	declare function rotate(ang As single) As vec2
	declare function rotate_center(x as single, y as single, ang As single) As vec2
End Type

function vec2.lensqr() as single
	return this.X * this.X + this.Y * this.Y
end function
Function vec2.len() As single
	return Sqr(this.lensqr)
End Function
Function vec2.add(v As vec2) As vec2
	return type(this.x + v.x, this.y + v.y)
End Function
Function vec2.subt(v As vec2) As vec2
	return type(this.X - v.X, this.Y - v.Y)
End Function
Function vec2.mul(s As single) As vec2
	return type(this.x*s, this.y*s)
End Function
Function vec2.dot(v As vec2) As single
  return this.x * v.x + this.y * v.y
End Function
Function vec2.cross(v As vec2) As single
  return this.x * v.y - this.y * v.x
End Function
Function vec2.norm() As vec2
  Dim As single s = 1 / this.len
  return type(this.x * s, this.y * s)
End Function
Function vec2.rotate(ang As single) As vec2
	Dim as single s,c
	c = cos(ang)
	s = sin(ang)
	return type(this.x*c - this.y*s, this.x*s + this.y*c)
End Function

Function vec2.rotate_center(x as single, y as single, ang As single) As vec2
	Dim as single s,c, dx,dy
	c = cos(ang)
	s = sin(ang)
	dx = this.x - x
	dy = this.y - y
	
	return type(x + dx*c - dy*s, y + dx*s + dy*c)
End Function
	
Namespace vector2
	Function lensqr(v As vec2) As single
	  return v.X * v.X + v.Y * v.Y
	End Function
	Function len(v As vec2) As single
	  return sqr(lensqr(v))
	End Function
	
	function create(x as single, y as single) as vec2
		return type(x,y)
	end function

	Function add(v As vec2, v2 As vec2) As vec2
		return type(v.X + v2.x, v.Y + v2.y)
	End Function
	Function subt(v1 As vec2, v2 As vec2) As vec2
		return type(v1.X - v2.X, v1.Y - v2.Y)
	End Function
	Function mul(v As vec2, s As single) As vec2
		return type(v.x*s, v.y*s)
	End Function

	Function dot(v1 As vec2, v2 As vec2) As single
	  return v1.x * v2.x + v1.y * v2.y
	End Function
	Function cross(v1 As vec2, v2 As vec2) As single
	  return v1.x * v2.y - v1.y * v2.x
	End Function
	function Clockwise(v1 As vec2, v2 As vec2) as long
		return (v1.x*v2.y - v1.y*v2.x) > 0
	end function 
	Function Distance(v1 As vec2, v2 as vec2) As single
	  return len(subt(v1,v2))
	End Function
	Function Norm(v As vec2) As vec2
	  Dim As single s = 1 / len(v)
	  return type(v.x * s, v.y * s)
	End Function
	
	Function Rotate(v As vec2, ang As single) As vec2
		Dim as single s,c
		c = cos(ang)
		s = sin(ang)
		return type(v.x*c - v.y*s, v.x*s + v.y*c)
	End Function
	Function RotateCS(v As vec2, c As single, s as single) As vec2
		return type(v.x*c - v.y*s, v.x*s + v.y*c)
	End Function
	
end namespace




'function Area(p1 as vec2, p2 as vec2, p3 as vec2) as single
'	return abs((p1.x*(p2.y-p3.y) + p2.x*(p3.y-p1.y)+ p3.x*(p1.y-p2.y))/2.0)
'end function

'function IsInsideTriangle(p1 as vec2,p2 as vec2, p3 as vec2 , p as vec2) as long 
'	dim as single a,a1,a2,a3
'	a =  area (p1,p2, p3)
'	a1 = area (p, p2, p3)
'	a2 = area (p1,p,  p3)
'	a3 = area (p1,p2, p)
'	return abs(a - (a1 + a2 + a3))<0.001
'end function


'Function IsOnLine(v1 As vec2, v2 As vec2, vS As vec2, ByVal rS As single) As Boolean
'  If v1.X = v2.X And v1.Y = v2.Y Then Exit Function
'  If v2_Dot(v2_Sub(vS, v1), v2_Sub(v2, v1)) < 0 Then
'    If v2_LenSq(v2_Sub(vS, v1)) < rS * rS Then IsOnLine = True: Exit Function
'  ElseIf v2_Dot(v2_Sub(vS, v2), v2_Sub(v1, v2)) < 0 Then
'    If v2_LenSq(v2_Sub(vS, v2)) < rS * rS Then IsOnLine = True: Exit Function
'  Else
'    If Abs(CCW(vNorm(v2_Sub(v2, v1)), v2_Sub(vS, v1))) < rS Then IsOnLine = True: Exit Function
'  End If
'End Function


'sub RectFromSector(cent As vec2, Radius as single, angle as single, FOV as single, byref r1 as vec2, byref r2 as vec2)
'	dim p(5) as vec2
	
'	p(0).x = cent.x: p(0).y = cent.y 
'	p(1).x = cent.x + cos(angle-FOV*0.5)*Radius: p(1).y = cent.y  + sin(angle-FOV*0.5)*Radius
'	p(2).x = cent.x + cos(angle+FOV*0.5)*Radius: p(2).y = cent.y  + sin(angle+FOV*0.5)*Radius
'	p(3).x = cent.x + cos(angle)*Radius: p(3).y = cent.y  + sin(angle)*Radius

'	p(4).x = cent.x + cos(angle-FOV*0.25)*Radius: p(4).y = cent.y  + sin(angle-FOV*0.25)*Radius
'	p(5).x = cent.x + cos(angle+FOV*0.25)*Radius: p(5).y = cent.y  + sin(angle+FOV*0.25)*Radius
	
'	dim as single minx=9999999, miny=99999999,maxx=-9999999,maxy=-9999999 
'	for n as long = 0 to 5
'		if p(n).x<minx then minx = p(n).x 
'		if p(n).y<miny then miny = p(n).y 
'		if p(n).x>maxx then maxx = p(n).x 
'		if p(n).y>maxy then maxy = p(n).y 
'	next
'	r1.x = minx: r1.y = miny
'	r2.x = maxx: r2.y = maxy
'end sub


'Function LineIntersection(p3 As vec2, p4 As vec2, p1 As vec2, p2 As vec2, crs As vec2) As Long
'    Dim t As single
'    Dim u As single
'    Dim den As single
'    den = (p1.x - p2.x) * (p3.y - p4.y) - (p1.y - p2.y) * (p3.x - p4.x)
'    If den = 0 Then return 0
'    t = ((p1.x - p3.x) * (p3.y - p4.y) - (p1.y - p3.y) * (p3.x - p4.x)) / den
'    u = -((p1.x - p2.x) * (p1.y - p3.y) - (p1.y - p2.y) * (p1.x - p3.x)) / den
'    'If t > 0 And t < 1 then
'        crs.x = p1.x + t * (p2.x - p1.x)
'        crs.y = p1.y + t * (p2.y - p1.y)
'        return  1
''    Else
''        return 0
''    End If
'End Function

'Function RayIntersection(p3 As vec2, p4 As vec2, p1 As vec2, p2 As vec2, crs As vec2) As Long
'    Dim t As single
'    Dim u As single
'    Dim den As single
'    den = (p1.x - p2.x) * (p3.y - p4.y) - (p1.y - p2.y) * (p3.x - p4.x)
'    If den = 0 Then return 0
'    t = ((p1.x - p3.x) * (p3.y - p4.y) - (p1.y - p3.y) * (p3.x - p4.x)) / den
'    u = -((p1.x - p2.x) * (p1.y - p3.y) - (p1.y - p2.y) * (p1.x - p3.x)) / den
'    If t > 0 And t < 1 And u > 0 Then
'        crs.x = p1.x + t * (p2.x - p1.x)
'        crs.y = p1.y + t * (p2.y - p1.y)
'        return  1
'    Else
'        return 0
'    End If
'End Function

'Function SegmentIntersection(p3 As vec2, p4 As vec2, p1 As vec2, p2 As vec2, crs As vec2) As Long
'    Dim t As single
'    Dim u As single
'    Dim den As single
'    den = (p1.x - p2.x) * (p3.y - p4.y) - (p1.y - p2.y) * (p3.x - p4.x)
'    If den = 0 Then return 0
'    t = ((p1.x - p3.x) * (p3.y - p4.y) - (p1.y - p3.y) * (p3.x - p4.x)) / den
'    u = -((p1.x - p2.x) * (p1.y - p3.y) - (p1.y - p2.y) * (p1.x - p3.x)) / den
'    If t > 0 And t < 1 And u > 0 and u<1 Then
'        crs.x = p1.x + t * (p2.x - p1.x)
'        crs.y = p1.y + t * (p2.y - p1.y)
'        return  1
'    Else
'        return 0
'    End If
'End Function

'function vMax(v1 as single,v2 as single) as single
'	if v1>v2 then return v1 else return v2
'end function
'function vMin(v1 as single,v2 as single) as single
'	if v1<v2 then return v1 else return v2	
'end function

'function distToSegmentSquared(v as vec2, w as vec2, p as vec2) as single
'	dim as single l2 = v2_LenSq(v2_Sub(v,w))
'	if l2=0 then return v2_LenSq(v2_Sub(p,v))
'	dim as single t = ((p.x-v.x)*(w.x-v.x) + (p.y-v.y)*(w.y-v.y))/l2
'	t = vmax(0,vmin(1,t))
'	dim as vec2 tp
'	tp.x=v.x+t*(w.x-v.x)
'	tp.y=v.y+t*(w.y-v.y)
'	return v2_LenSq(v2_Sub(p,tp))
'end function





'Function AABB(ByVal x1 As single, ByVal y1 As single, ByVal x2 As single, ByVal y2 As single, _
'              ByVal a1 As single, ByVal B1 As single, ByVal a2 As single, ByVal B2 As single) As Long

'    If x1 > x2 Then swap x1,x2 
'    If y1 > y2 Then swap y1,y2 
'    If a1 > a2 Then swap a1,a2
'    If B1 > B2 Then swap b1,b2
    
'    If y1 > B2 Then return 0
'    If B1 > y2 Then return 0
'    If x1 > a2 Then return 0
'    If a1 > x2 Then return 0
    
'    return 1
'End Function


'Function PointInRect(start1 As vec2, end1 As vec2, pnt As vec2) As Long
'    Dim x1 As single, y1 As single, x2 As single, y2 As single
'    If start1.x > end1.x Then
'        x1 = end1.x
'        x2 = start1.x
'    Else
'        x1 = start1.x
'        x2 = end1.x
'    End If
'    If start1.y > end1.y Then
'        y1 = end1.y
'        y2 = start1.y
'    Else
'        y1 = start1.y
'        y2 = end1.y
'    End If

'	If Abs(start1.y - end1.y) < 0.01 Then
'		If pnt.x >= x1 And pnt.x <= x2 Then
'			PointInRect = 1
'		Else
'			PointInRect = 0
'		End If
'	Else
'		If pnt.x >= x1 And pnt.y >= y1 And pnt.x <= x2 And pnt.y <= y2 Then
'			PointInRect = 1
'		Else
'			PointInRect = 0
'		End If
'	End If
'End Function

'Function LineIntersectionT(s1 As vec2, e1 As vec2, s2 As vec2, e2 As vec2, byref crs As vec2) As Long
'    'If AABB(s1.x, s1.y, e1.x, e1.y, s2.x, s2.y, e2.x, e2.y) = 0 Then Exit Function
'    If s1.y = e1.y And s2.y = e2.y Then return 0
    
'    Dim cx As single, cy As single
'    Dim a1 As single, B1 As single, a2 As single, B2 As single
'    Dim x1 As single, x2 As single, y1 As single, y2 As single
'    If s1.x = e1.x Then
'        If s2.x = e2.x Then return 0
'        a2 = (e2.y - s2.y) / (e2.x - s2.x)
'        B2 = s2.y - a2 * s2.x
'        crs.x = e1.x
'        crs.y = a2 * crs.x + B2

'    ElseIf s2.x = e2.x Then
'        a1 = (e1.y - s1.y) / (e1.x - s1.x)
'        B1 = s1.y - a1 * s1.x
'        crs.x = e2.x
'        crs.y = a1 * crs.x + B1
'    Else
'        a1 = (e1.y - s1.y) / (e1.x - s1.x)
'        B1 = s1.y - a1 * s1.x

'        a2 = (e2.y - s2.y) / (e2.x - s2.x)
'        B2 = s2.y - a2 * s2.x

'        If a1 = a2 Then return 0

'        crs.x = (B2 - B1) / (a1 - a2)
'        crs.y = a1 * crs.x + B1
'    End If
    
'	If PointInRect(s1, e1, crs) Then
'		If PointInRect(s2, e2, crs) Then
'			return 1
'		End If
'	End If
        
'End Function