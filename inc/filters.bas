

sub im_Bumpf(src as single ptr, tmp as single ptr, w as long, h as long)
	memcpy tmp, src, w*h*4
	dim as long i = 0, x1,y1,x2,y2
	for y as long = 0 to h-1
		for x as long = 0 to w-1
			
			x1 = x-10: y1=y-10
			x2 = x+10: y2=y+10
			if x1<0 then x1=0
			if y1<0 then y1=0
			if x2>w-1 then x2=w-1
			if y2>h-1 then y2=h-1
			
			dim as single avg=0
			for b as long = y1 to y2
			for a as long = x1 to x2
				avg += tmp[a+b*w]
			next
			next
			avg /= ((x2-x1+1)*(y2-y1+1))
			
			src[i] = tmp[i] - avg 

			i+=1
		next
	next
end sub

sub im_Noise(s as single ptr, w as long, h as long, noise_k as single)
	dim as long i = 0
	for y as long = 0 to h-1
		for x as long = 0 to w-1
			s[i] = s[i] + rnd*noise_k - noise_k*0.5
			i+=1
		next
	next
end sub



sub imBlurf(s as single ptr, w as long, h as long)
	dim as single ptr tim = allocate(w*h*sizeof(single))
	memcpy tim, s, w*h*sizeof(single)
	
	dim as long i
	dim as single v
	for y as long = 1 to h-2
		for x as long = 1 to w-2
			i = x+y*w
			v = (tim[i] + tim[i-1] + tim[i+1] + tim[i-w] + tim[i+w])*0.2
			
			s[i] = v
		next
	next
	
	deallocate(tim)
end sub

sub im2float(s as IMAGE, d as single ptr)
	dim as long i = 0, k = 0
	for y as long = 0 to s.h-1
		for x as long = 0 to s.w-1
			d[k+0] = csng(s.dt[i+0]) * 0.003921568
			d[k+1] = csng(s.dt[i+1]) * 0.003921568
			d[k+2] = csng(s.dt[i+2]) * 0.003921568
			i+=4
			k+=3
		next
	next
end sub

sub im2NPalign(byref s as single ptr, w as long, h as long)
	dim as long i = 0
	for n as long = 0 to w*h-1
		s[n] = s[n] - 0.5
	next
end sub
sub im2Greyf(s as IMAGE, d as single ptr)
	dim as long i = 0
	for y as long = 0 to s.h-1
		for x as long = 0 to s.w-1
			'd[i\4] = (csng(s.dt[i]) + csng(s.dt[i+1]) + csng(s.dt[i+2])) * 0.00261438 - 1   ' -1 to 1
			d[i\4] = (csng(s.dt[i]) + csng(s.dt[i+1]) + csng(s.dt[i+2])) * 0.00130719  ' 0 to 1
			i+=4
		next
	next
end sub
sub imDownscalef(s as single ptr, d as single ptr, w as long, h as long)
	dim as long dw,dh
	dw = w\2
	dh = h\2
	dim as long i, k=0
	for y as long = 0 to dh-1
		for x as long = 0 to dw-1
			i = x*2 + (y*2)*w
			
			d[k] = s[i]
			k+=1
		next
	next
end sub
sub imDownscale2(s as single ptr, d as single ptr, w as long, h as long)
	dim as long dw,dh
	dw = w\2
	dh = h\2
	dim as long i, k=0
	for y as long = 0 to dh-1
		for x as long = 0 to dw-1
			i = x*2 + (y*2)*w
			d[k] = (s[i] + s[i+1] + s[i+w] + s[i+w+1])*0.25
			k+=1
		next
	next
end sub

sub imDownscale(s as single ptr, d as single ptr, w as long, h as long, scale as long)
	dim as long dw,dh
	dw = w\scale
	dh = h\scale
	dim as long i, k=0
	for y as long = 0 to dh-1
		for x as long = 0 to dw-1
			
			d[k] = 0
			for b as long = 0 to scale-1
				for a as long = 0 to scale-1
					i = x*scale+a + (y*scale+b)*w
					d[k] += s[i]
				next
			next
			d[k] = d[k] / (scale*scale)
			
			k+=1
		next
	next
end sub


sub imLBP(s as single ptr, d as single ptr, w as long, h as long)
	dim as long i, k=0, v
	dim as single mv
	for y as long = 1 to h-2
		for x as long = 1 to w-2
			
			mv = s[x+y*w]
			
			v=0
			if s[(x-1)+(y-1)*w]>mv then v+=1
			if s[(x)  +(y-1)*w]>mv then v+=2
			if s[(x+1)+(y-1)*w]>mv then v+=4
			if s[(x-1)+(y)*w]>mv then v+=8
			if s[(x+1)+(y)*w]>mv then v+=16
			if s[(x-1)+(y+1)*w]>mv then v+=32
			if s[(x)  +(y+1)*w]>mv then v+=64
			if s[(x+1)+(y+1)*w]>mv then v+=128
			d[i] = v * 0.00392156
			
			i+=1
		next
	next
end sub



