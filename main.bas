#include "inc/gdip.bas"

'#define Floor(d) (Int(d))
'#define Ceil(d) (-Int(-d))

#ifndef min
#macro min(a,b) iif((a)<(b),(a),(b)) #endmacro
#macro max(a,b) iif((a)>(b),(a),(b)) #endmacro
#endif
#macro min3(a,b,c)   min(min((a),(b)),(c)) #endmacro
#macro min4(a,b,c,d) min(min3((a),(b),(c)),(d)) #endmacro
#macro max3(a,b,c)   max(max((a),(b)),(c)) #endmacro
#macro max4(a,b,c,d) max(max3((a),(b),(c)),(d)) #endmacro

#include "inc/vec2.bas"

type color_t
	as ubyte r,g,b
end type

type vertex
	as vec2 	p
	as color_t 	col
	as vec2 	uv
end type

dim shared as IMAGE img


function is_top_left(p1 as vec2, p2 as vec2) as long
	dim as vec2 edge = p2.subt(p1)
	dim as long is_top_edge  = (edge.y = 0 and edge.x > 0)
	dim as long is_left_edge = (edge.y < 0)
	return (is_left_edge or is_top_edge)
end function

function edge_cross(a as vec2, b as vec2, p as vec2) as single
	dim as vec2 ab = b.subt(a)
	dim as vec2 ap = p.subt(a)
	return ab.x * ap.y - ab.y * ap.x ' cross prod
end function

sub draw_triangle(byval v0 as vertex, byval v1 as vertex, byval v2 as vertex)
	'// Finds the bounding box with all candidate pixels
	dim as long x_min = min3(v0.p.x, v1.p.x, v2.p.x)
	dim as long y_min = min3(v0.p.y, v1.p.y, v2.p.y)
	dim as long x_max = max3(v0.p.x, v1.p.x, v2.p.x)
	dim as long y_max = max3(v0.p.y, v1.p.y, v2.p.y)

	'// Compute the area of the entire triangle/parallelogram
	dim as single area = edge_cross(v0.p,v1.p,v2.p)

	' clockwise triangle
	if area<0 then
		swap v1,v2
		area=-area
	end if
	
	'// Compute the constant delta_s that will be used for the horizontal and vertical steps
	dim as single d_w0_x = (v1.p.y - v2.p.y)
	dim as single d_w1_x = (v2.p.y - v0.p.y)
	dim as single d_w2_x = (v0.p.y - v1.p.y)
	dim as single d_w0_y = (v2.p.x - v1.p.x)
	dim as single d_w1_y = (v0.p.x - v2.p.x)
	dim as single d_w2_y = (v1.p.x - v0.p.x)
	
	'// Rasterization fill rule, not 100% precise due to floating point innacuracy
	dim as single  bias0 = iif(is_top_left(v1.p, v2.p) , 0 , -0.5)
	dim as single  bias1 = iif(is_top_left(v2.p, v0.p) , 0 , -0.5)
	dim as single  bias2 = iif(is_top_left(v0.p, v1.p) , 0 , -0.5)
	
	'// Compute the edge functions for the fist (top-left) point
	dim as vec2 p0 = type(x_min+0.5, y_min+0.5)
	dim as single w0_y = edge_cross(v1.p, v2.p, p0) + bias0
	dim as single w1_y = edge_cross(v2.p, v0.p, p0) + bias1
	dim as single w2_y = edge_cross(v0.p, v1.p, p0) + bias2
	
	
	for y as long = y_min to y_max
		dim as single w0 = w0_y
		dim as single w1 = w1_y
		dim as single w2 = w2_y
		for x as long = x_min to x_max
			dim as long   inside = w0>=0 and w1>=0 and w2>=0
			if inside then
				dim as single alpha = (w0) / (area)
				dim as single beta  = (w1) / (area)
				dim as single gamma = (w2) / (area)
				
				dim as long u, v
				dim as long r
				dim as long g
				dim as long b
				
				' color
'				r = (alpha)*v0.col.r + (beta)*v1.col.r + (gamma)*v2.col.r
'				g = (alpha)*v0.col.g + (beta)*v1.col.g + (gamma)*v2.col.g
'				b = (alpha)*v0.col.b + (beta)*v1.col.b + (gamma)*v2.col.b
				
				' texture
'				u = floor( abs((alpha)*v0.uv.x + (beta)*v1.uv.x + (gamma)*v2.uv.x )) mod img.w
'				v = floor( abs((alpha)*v0.uv.y + (beta)*v1.uv.y + (gamma)*v2.uv.y) ) mod img.h
				
				dim as single interpolated_reciprocal_w, interpolated_u , interpolated_v
				dim as single kkw = 0.5
				
				interpolated_reciprocal_w = alpha /1 +  beta/1 +  gamma/kkw
				
				if interpolated_reciprocal_w > 0 then 
					interpolated_u =  (alpha)*(v0.uv.x/1) + (beta)*(v1.uv.x/1) + (gamma)*(v2.uv.x/kkw)
					interpolated_v =  (alpha)*(v0.uv.y/1) + (beta)*(v1.uv.y/1) + (gamma)*(v2.uv.y/kkw)
					interpolated_u /= interpolated_reciprocal_w
					interpolated_v /= interpolated_reciprocal_w
					
					u = floor(abs(interpolated_u)) mod img.w
					v = floor(abs(interpolated_v)) mod img.h
					
					r = img.dt[ (u+v*img.w)*4 +2 ]
					g = img.dt[ (u+v*img.w)*4 +1 ]
					b = img.dt[ (u+v*img.w)*4 +0 ]
					
					pset(x,y), rgb(r,g,b)
				
				end if
			end if
			w0 += d_w0_x
			w1 += d_w1_x
			w2 += d_w2_x
		next
		w0_y += d_w0_y
		w1_y += d_w1_y
		w2_y += d_w2_y
	next
	
	'line(x_min,y_min) - (x_max,y_max),,b

end sub



'sub draw_triangleSL(byval v0 as vertex, byval v1 as vertex, byval v2 as vertex)
'	'// Finds the bounding box with all candidate pixels
'	dim as long x_min = min3(v0.p.x, v1.p.x, v2.p.x)
'	dim as long y_min = min3(v0.p.y, v1.p.y, v2.p.y)
'	dim as long x_max = max3(v0.p.x, v1.p.x, v2.p.x)
'	dim as long y_max = max3(v0.p.y, v1.p.y, v2.p.y)
'	'// Compute the area of the entire triangle/parallelogram
'	dim as single area = edge_cross(v0.p,v1.p,v2.p)
'	' clockwise triangle
'	if area<0 then
'		swap v1,v2
'		area=-area
'	end if
'	'// Compute the constant delta_s that will be used for the horizontal and vertical steps
'	dim as single d_w0_x = (v1.p.y - v2.p.y)
'	dim as single d_w1_x = (v2.p.y - v0.p.y)
'	dim as single d_w2_x = (v0.p.y - v1.p.y)
'	dim as single d_w0_y = (v2.p.x - v1.p.x)
'	dim as single d_w1_y = (v0.p.x - v2.p.x)
'	dim as single d_w2_y = (v1.p.x - v0.p.x)
'	'// Rasterization fill rule, not 100% precise due to floating point innacuracy
'	dim as single  bias0 = iif(is_top_left(v1.p, v2.p) , 0 , -0.5)
'	dim as single  bias1 = iif(is_top_left(v2.p, v0.p) , 0 , -0.5)
'	dim as single  bias2 = iif(is_top_left(v0.p, v1.p) , 0 , -0.5)
'	'// Compute the edge functions for the fist (top-left) point
'	dim as vec2 p0 = type(x_min+0.5, y_min+0.5)
'	dim as single w0_y = edge_cross(v1.p, v2.p, p0) + bias0
'	dim as single w1_y = edge_cross(v2.p, v0.p, p0) + bias1
'	dim as single w2_y = edge_cross(v0.p, v1.p, p0) + bias2
'	dim as single w0 = w0_y
'	dim as single w1 = w1_y
'	dim as single w2 = w2_y
'	dim as long x = x_min, last_inside, ws = 1

'	for y as long = y_min to y_max
'		last_inside = 0
'		do
'			dim as long   inside = w0>=0 and w1>=0 and w2>=0
'			if inside then
'				dim as single alpha = (w0) / (area)
'				dim as single beta  = (w1) / (area)
'				dim as single gamma = (w2) / (area)
'				dim as long u, v
'				dim as long r
'				dim as long g
'				dim as long b
'				dim as single interpolated_reciprocal_w, interpolated_u , interpolated_v
'				dim as single kkw = 0.5
'				interpolated_reciprocal_w = alpha /1 +  beta/1 +  gamma/kkw
'				'if interpolated_reciprocal_w > 0 then 
'					interpolated_u = (alpha)*(v0.uv.x/1) + (beta)*(v1.uv.x/1) + (gamma)*(v2.uv.x/kkw)
'					interpolated_v = (alpha)*(v0.uv.y/1) + (beta)*(v1.uv.y/1) + (gamma)*(v2.uv.y/kkw)
'					interpolated_u /= interpolated_reciprocal_w
'					interpolated_v /= interpolated_reciprocal_w
'					u = floor(abs(interpolated_u)) mod img.w
'					v = floor(abs(interpolated_v)) mod img.h
'					r = img.dt[ (u+v*img.w)*4 +2 ]
'					g = img.dt[ (u+v*img.w)*4 +1 ]
'					b = img.dt[ (u+v*img.w)*4 +0 ]
'					pset(x,y),  rgb(r,g,b)
					
'				'end if
'			end if

'			w0 += d_w0_x
'			w1 += d_w1_x
'			w2 += d_w2_x
'			x+=ws

'			' end of bounding box or end of triange scanline
'			'if x = x_max-1 or (last_inside<>0 and inside=0) then
'			if (x > x_max and ws>0) or (x < x_min and ws<0) or (last_inside<>0 and inside=0) then
			
'				w0 += d_w0_x*32
'				w1 += d_w1_x*32
'				w2 += d_w2_x*32
'				x+= ws*32

'				' next scnline				
'				ws=-ws			' reverse scan direction
'				d_w0_x=-d_w0_x
'				d_w1_x=-d_w1_x
'				d_w2_x=-d_w2_x
'				w0 += d_w0_y	' next line
'				w1 += d_w1_y
'				w2 += d_w2_y
				
'				exit do
'			end if

'			last_inside = inside
			
'		loop 

'	next
'end sub




dim as vec2 vertices(2) => {type(40*3,40*3), type(80*3,40*3) , type(40*3,80*3)}
dim as color_t colr(2) => { type(&HFF,&H00,&H00), type(&H00,&HFF,&H00), type(&H00,&H00,&HFF) }


img.load "txt.bmp"

screenres 320,320,32

dim as vertex v1,v2,v3
v1.p=vertices(0)
v2.p=vertices(1)
v3.p=vertices(2)

v1.col = colr(0)
v2.col = colr(1)
v3.col = colr(2)

'v1.uv = type(-0.5,-0.5)
'v2.uv = type(15.5,-0.5)
'v3.uv = type(-0.5,15.5)

v1.uv = type(0,0)
v2.uv = type(16,0)
v3.uv = type(0,16)

do
	dim as long msx,msy,msb
	getmouse msx,msy,,msb
	if msb<>0 and msx<>-1 then
		v1.p = type(msx,msy)
	end if
	
'	v1 = vertices(0).rotate_center(160, 160, timer)
'	v2 = vertices(1).rotate_center(160, 160, timer)
'	v3 = vertices(2).rotate_center(160, 160, timer)

	screenlock 
		cls 
		draw_triangle(v1,v2,v3)
	screenunlock
	
	sleep 10
loop

sleep



