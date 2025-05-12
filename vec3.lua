AddCSLuaFile()

local DBL_EPSILON = 2.22044604925031308e-16

local mt = {}
function mt.__tostring(v)
    return v[1] .. ", " ..  v[2] .. ", " .. v[3]
    --return string.format("%0.12f, %0.12f, %0.12f", v[1], v[2], v[3])
end

local function new(x,y,z)

    if istable(x) then
        return setmetatable({x[1], x[2], x[3]}, mt)
    elseif x then
        return setmetatable({x,y,z}, mt)
    else
        return setmetatable({0,0,0}, mt)
    end

end

local function ident(a)

    a[1], a[2], a[3] = 0, 0, 0
    return q

end

local function setv(a,b)

    a[1], a[2], a[3] = b[1], b[2], b[3] 
    return a

end

local function set(a,x,y,z)

    a[1], a[2], a[3] = x, y, z
    return a

end

local function unp(a)

    return a[1], a[2], a[3]

end

local __min = math.min
local __max = math.max
local function min(a,b,c)

    c[1] = __min(a[1],b[1])
    c[2] = __min(a[2],b[2])
    c[3] = __min(a[3],b[3])
    return c

end

local function max(a,b,c)

    c[1] = __max(a[1],b[1])
    c[2] = __max(a[2],b[2])
    c[3] = __max(a[3],b[3])
    return c

end

local __abs = math.abs
local function abs(a,c)

    c[1] = __abs(a[1])
    c[2] = __abs(a[2])
    c[3] = __abs(a[3])
    return c

end

local function add(a,b,c)

    c[1] = a[1] + b[1]
    c[2] = a[2] + b[2]
    c[3] = a[3] + b[3]
    return c

end

local function sub(a,b,c)

    c[1] = a[1] - b[1]
    c[2] = a[2] - b[2]
    c[3] = a[3] - b[3]
    return c

end

local function mul(a,b,c)

    c[1] = a[1] * b[1]
    c[2] = a[2] * b[2]
    c[3] = a[3] * b[3]
    return c

end

local function div(a,b,c)

    c[1] = a[1] / b[1]
    c[2] = a[2] / b[2]
    c[3] = a[3] / b[3]
    return c

end

local function reciprocal(a,c)

    c[1] = 1 / a[1]
    c[2] = 1 / a[2]
    c[3] = 1 / a[3]
    return c

end

local function ma(a,b,s,c)

    c[1] = a[1] + b[1] * s
    c[2] = a[2] + b[2] * s
    c[3] = a[3] + b[3] * s
    return c

end

local function lerp(a,b,f,c)

    c[1] = a[1] * (1-f) + b[1] * f
    c[2] = a[2] * (1-f) + b[2] * f
    c[3] = a[3] * (1-f) + b[3] * f
    return c

end

local function dot(a,b) return a[1] * b[1] + a[2] * b[2] + a[3] * b[3] end
local function len_sqr(a) return a[1] ^ 2 + a[2] ^2 + a[3] ^ 2 end
local function len(a) return math.sqrt(a[1] ^ 2 + a[2] ^ 2 + a[3] ^ 2) end

local function dist_sqr(a,b)

    local x = b[1] - a[1]
    local y = b[2] - a[2]
    local z = b[3] - a[3]
    return (x ^ 2 + y ^ 2 + z ^ 2)

end

local function dist(a,b)

    local x = b[1] - a[1]
    local y = b[2] - a[2]
    local z = b[3] - a[3]
    return math.sqrt(x ^ 2 + y ^ 2 + z ^ 2)

end

local function scale(a,s,c)

    c[1] = a[1] * s
    c[2] = a[2] * s
    c[3] = a[3] * s
    return c

end

local function sign(a,c)

    c[1] = a[1] >= 0 and 1 or -1
    c[2] = a[2] >= 0 and 1 or -1
    c[3] = a[3] >= 0 and 1 or -1
    return c

end

local function is_normalized(a,t)
    return math.abs(len(a) - 1) < (t or 1e-12)
end

local function normalized(a,c)
    c = c or a
    local l = len(a)
    c[1] = a[1] / l
    c[2] = a[2] / l
    c[3] = a[3] / l
    return c
end

local function normalized_or(a,o,c)
    local l = len(a)
    if l == 0 then
        setv(c, o)
    else
        c[1] = a[1] / l
        c[2] = a[2] / l
        c[3] = a[3] / l
    end
    return c
end

local function normalized_or_zero(a,c)
    local l = len(a)
    if l == 0 then
        set(c,0,0,0)
    else
        c[1] = a[1] / l
        c[2] = a[2] / l
        c[3] = a[3] / l
    end
    return c
end

local function perpendicular(a,c)
    if math.abs(a[1]) > math.abs(a[2]) then
        local len = math.sqrt(a[1] * a[1] + a[3] * a[3])
        local inv_len = 1/len
        c[1] = a[3] * inv_len
        c[2] = 0
        c[3] = -a[1] * inv_len
        return c
    else
        local len = math.sqrt(a[2] * a[2] + a[3] * a[3])
        local inv_len = 1/len
        c[1] = 0
        c[2] = a[3] * inv_len
        c[3] = -a[2] * inv_len
        return c
    end
end

local function cross(a,b,c)

    c[1], c[2], c[3] = 
    a[2] * b[3] - a[3] * b[2],
    a[3] * b[1] - a[1] * b[3],
    a[1] * b[2] - a[2] * b[1]

    return c

end

local function compare(a,b,t)

    if t then
        return math.abs(a[1] - b[1]) < t
        and    math.abs(a[2] - b[2]) < t
        and    math.abs(a[3] - b[3]) < t
    end
    return a[1] == b[1] and a[2] == b[2] and a[3] == b[3]

end

local function unit_spherical(a,theta,phi)

    local ct = math.cos(theta)
    local st = math.sin(theta)
    local cp = math.cos(phi)
    local sp = math.sin(phi)
    a[1] = st * cp
    a[2] = st * sp
    a[3] = ct
    return a

end

local function random(a)

    local theta = math.pi * math.random()
    local phi = 2 * math.pi * math.random()
    return unit_spherical(a,theta,phi)

end

local function intersect_aabb(org, dir, min, max, out_normal)

    local x0,y0,z0 = unp(min)
    local x1,y1,z1 = unp(max)
    local ox,oy,oz = unp(org)
    local dx,dy,dz = unp(dir)

    dx = 1/dx
    dy = 1/dy
    dz = 1/dz

    local t0 = (x0 - ox) * dx
    local t1 = (x1 - ox) * dx
    local t2 = (y0 - oy) * dy
    local t3 = (y1 - oy) * dy
    local t4 = (z0 - oz) * dz
    local t5 = (z1 - oz) * dz

    local tmin = 
    math.max(
        math.max(
            math.min(t0,t1),
            math.min(t2,t3)
        ),
        math.min(t4,t5)
    )

    local tmax = 
    math.min(
        math.min(
            math.max(t0,t1),
            math.max(t2,t3)
        ),
        math.max(t4,t5)
    )

    if tmax < 0 then return false end
    if tmin > tmax then return false end

    if out_normal then
        if tmin == t0 then set(out_normal, -1, 0, 0) end
        if tmin == t1 then set(out_normal, 1, 0, 0) end
        if tmin == t2 then set(out_normal, 0, -1, 0) end
        if tmin == t3 then set(out_normal, 0, 1, 0) end
        if tmin == t4 then set(out_normal, 0, 0, -1) end
        if tmin == t5 then set(out_normal, 0, 0, 1) end
    end

    return true, tmin, tmax

end

local function lowest_component_index(a)

    return a[1] < a[2] and (a[3] < a[1] and 3 or 1) or (a[3] < a[2] and 3 or 2)

end

local function highest_component_index(a)

    return a[1] > a[2] and (a[3] > a[1] and 3 or 1) or (a[3] > a[2] and 3 or 2)

end

local __tmp0 = {}
local __tmp1 = {}
local __tmp2 = {}

local function to_barycentric2(a, b, p)

    if p then 
        a = sub(a, p, __tmp1)
        b = sub(b, p, __tmp2)
    end

    local ab = sub(b, a, __tmp0)
    local denom = len_sqr(ab)
    if denom < DBL_EPSILON * DBL_EPSILON then
        if len_sqr(a) < len_sqr(b) then
            return 1, 0
        else
            return 0, 1
        end
    else
        local v = -dot(a,ab) / denom
        return 1 - v, v
    end

end

local function from_barycentric2(v0, v1, u, v, out)

    out[1] = v0[1] * u + v1[1] * v
    out[2] = v0[2] * u + v1[2] * v
    out[3] = v0[3] * u + v1[3] * v
    return out

end

local __tmp0 = {}
local __tmp1 = {}
local __tmp2 = {}
local __tmp3 = {}
local __tmp4 = {}
local __tmp5 = {}

local function to_barycentric3(a, b, c, p)

    if p then 
        a = sub(a, p, __tmp3)
        b = sub(b, p, __tmp4)
        c = sub(c, p, __tmp5)
    end

    local v0 = sub(b, a, __tmp0)
    local v1 = sub(c, a, __tmp1)
    local v2 = sub(c, b, __tmp2)

    local d00 = len_sqr(v0)
    local d11 = len_sqr(v1)
    local d22 = len_sqr(v2)

    if d00 <= d22 then

        local d01 = dot(v0, v1)
        local denom = d00 * d11 - d01 * d01
        if math.abs(denom) < DBL_EPSILON then
            if d00 > d11 then
                local u,v = get_barycentric2(a, b)
                return u, v, 0
            else
                local u,v = get_barycentric2(a, c)
                return u, v, 0
            end
        else
            local a0 = dot(a, v0)
            local a1 = dot(a, v1)
            local v = (d01 * a1 - d11 * a0) / denom
            local w = (d01 * a0 - d00 * a1) / denom
            local u = 1 - v - w
            return u,v,w
        end

    else

        local d12 = dot(v1, v2)
        local denom = d11 * d22 - d12 * d12
        if math.abs(denom) < DBL_EPSILON then
            if d11 > d22 then
                local u,v = get_barycentric2(a, c)
                return u, v, 0
            else
                local u,v = get_barycentric2(b, c)
                return u, v, 0
            end
        else
            local c1 = dot(c, v1)
            local c2 = dot(c, v2)
            local u = (d22 * c1 - d12 * c2) / denom
            local v = (d11 * c2 - d12 * c1) / denom
            local w = 1 - u - v
            return u,v,w
        end

    end

end

local function from_barycentric3(v0, v1, v2, u, v, w, out)

    out[1] = v0[1] * u + v1[1] * v + v2[1] * w
    out[2] = v0[2] * u + v1[2] * v + v2[2] * w
    out[3] = v0[3] * u + v1[3] * v + v2[3] * w
    return out

end

local function to_gmod(a,vec)

    if not vec then
        vec = Vector(a[1], a[2], a[3])
    else
        vec:SetUnpacked(a[1], a[2], a[3])
    end
    return vec

end

local function from_gmod(a,vec)

    if not a then
        local x,y,z = vec:Unpack()
        a = new(x,y,z)
    else
        a[1], a[2], a[3] = vec:Unpack()
    end
    return a

end

-- networking
local coord_intbits = 14
local coord_divbits = 5
local coord_denom = bit.lshift(1, coord_divbits)
local coord_res = 1/coord_denom
local norm_bits = 11
local norm_denom = bit.lshift(1, norm_bits) - 1
local norm_res = 1/norm_denom

local __floor = math.floor
local __band = bit.band

local function net_write_pos_component(x)

    local int = __floor(__abs(x))
    local div = __band(__floor(__abs(x * coord_denom)), coord_denom - 1)
    local cmp = (int ~= 0 and 1 or 0) + (div ~= 0 and 2 or 0)

    net.WriteUInt(cmp, 2)
    if int ~= 0 then net.WriteUInt(int-1, coord_intbits) end
    if div ~= 0 then net.WriteUInt(div, coord_divbits) end
    net.WriteBool(x <= -coord_res)

end

local function net_read_pos_component()

    local cmp, x = net.ReadUInt(2), 0
    if cmp % 2 ~= 0 then x = x + net.ReadUInt(coord_intbits) + 1 end
    if cmp > 1 then x = x + net.ReadUInt(coord_divbits) * coord_res end
    return net.ReadBool() and -x or x

end

local function net_write_norm_component(x)

    net.WriteUInt(__min(__floor(__abs(x * norm_denom)), norm_denom), norm_bits)
    net.WriteBool(x <= -norm_res)

end

local function net_read_norm_component()

    return (net.ReadUInt(norm_bits) * norm_res) * (net.ReadBool() and -1 or 1)

end

local function net_write_pos(a)

    for i=1, 3 do
        local p = a[i] >= coord_res or a[i] <= -coord_res
        net.WriteBool(p) if p then net_write_pos_component(a[i]) end
    end

end

local function net_read_pos(a)

    a = a or new()
    for i=1, 3 do a[i] = net.ReadBool() and net_read_pos_component() or 0 end
    return a

end

local function net_write_normal(a)

    for i=1, 2 do
        local p = a[i] >= norm_res or a[i] <= -norm_res
        net.WriteBool(p) if p then net_write_norm_component(a[i]) end
    end
    net.WriteBool(a[3] <= -norm_res)

end

local function net_read_normal(a)

    a = a or new()
    for i=1, 2 do a[i] = net.ReadBool() and net_read_norm_component() or 0 end
    local z = a[1]^2 + a[2]^2
    a[3] = (z < 1 and math.sqrt(1-z) or 0) * (net.ReadBool() and -1 or 1)
    return a

end

return {
    zero = new(0,0,0),
    one = new(1,1,1),
    x = new(1,0,0),
    y = new(0,1,0),
    z = new(0,0,1),
    nx = new(-1,0,0),
    ny = new(0,-1,0),
    nz = new(0,0,-1),
    new = new,
    ident = ident,
    setv = setv,
    set = set,
    unp = unp,
    abs = abs,
    min = min,
    max = max,
    add = add,
    sub = sub,
    mul = mul,
    div = div,
    dot = dot,
    reciprocal = reciprocal,
    ma = ma,
    lerp = lerp,
    len_sqr = len_sqr,
    len = len,
    dist_sqr = dist_sqr,
    dist = dist,
    scale = scale,
    sign = sign,
    is_normalized = is_normalized,
    normalized = normalized,
    normalized_or = normalized_or,
    normalized_or_zero = normalized_or_zero,
    perpendicular = perpendicular,
    cross = cross,
    compare = compare,
    unit_spherical = unit_spherical,
    random = random,
    intersect_aabb = intersect_aabb,
    lowest_component_index = lowest_component_index,
    highest_component_index = highest_component_index,
    to_barycentric2 = to_barycentric2,
    from_barycentric2 = from_barycentric2,
    to_barycentric3 = to_barycentric3,
    from_barycentric3 = from_barycentric3,
    to_gmod = to_gmod,
    from_gmod = from_gmod,
    net_read_pos = net_read_pos,
    net_write_pos = net_write_pos,
    net_read_normal = net_read_normal,
    net_write_normal = net_write_normal,
}