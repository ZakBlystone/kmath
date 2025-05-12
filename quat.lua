AddCSLuaFile()

local kvec3 = include "vec3.lua"

local tq = {}
local tq2 = {}

local function new(x,y,z,w)

    if type(x) == "table" then
        return {x[1], x[2], x[3], x[4]}
    elseif x then
        return {x,y,z,w}
    else
        return {0,0,0,1}
    end

end

local function ident(q)

    q[1], q[2], q[3], q[4] = 0, 0, 0, 1
    return q

end

local function set(q,x,y,z,w)

    q[1], q[2], q[3], q[4] = x, y, z, w
    return q

end

local function setq(q,o)

    q[1], q[2], q[3], q[4] = o[1], o[2], o[3], o[4]
    return q

end

local function add(a,b,c)

    c[1] = a[1] + b[1]
    c[2] = a[2] + b[2]
    c[3] = a[3] + b[3]
    c[4] = a[4] + b[4]
    return c

end

local function sub(a,b,c)

    c[1] = a[1] - b[1]
    c[2] = a[2] - b[2]
    c[3] = a[3] - b[3]
    c[4] = a[4] - b[4]
    return c

end

local function dot(a,b)

    return a[1] * b[1] + a[2] * b[2] + a[3] * b[3] + a[4] * b[4]

end

local function conjugate(q,o)

    o[1] = -q[1]
    o[2] = -q[2]
    o[3] = -q[3]
    o[4] = q[4]
    return o

end

local function mul(a,b,c)

    local lx,ly,lz,lw = a[1], a[2], a[3], a[4]
    local rx,ry,rz,rw = b[1], b[2], b[3], b[4]
    c[1] = lw * rx + lx * rw + ly * rz - lz * ry
    c[2] = lw * ry - lx * rz + ly * rw + lz * rx
    c[3] = lw * rz + lx * ry - ly * rx + lz * rw
    c[4] = lw * rw - lx * rx - ly * ry - lz * rz
    return c

end

local function mul_vec(q,v,c)

    setq(tq, v)
    tq[4] = 0

    mul(q, tq, tq)
    conjugate(q, tq2)
    mul(tq, tq2, tq)

    kvec3.setv(c, tq)
    return c

end

local function len_sqr(q) return q[1] ^ 2 + q[2] ^2 + q[3] ^ 2 + q[4] ^ 2 end
local function len(q) 
    return math.sqrt(q[1] ^ 2 + q[2] ^2 + q[3] ^ 2 + q[4] ^ 2) 
end

local function normalized(q,c)

    local l = len(q)
    local il = 1/l
    c[1] = q[1] * il
    c[2] = q[2] * il
    c[3] = q[3] * il
    c[4] = q[4] * il
    return c

end

local function is_normalized(q,t)

    t = t or 1e-5
    return math.abs(len_sqr(q) - 1) <= t

end

local function scale(q,s,c)

    c[1] = q[1] * s
    c[2] = q[2] * s
    c[3] = q[3] * s
    c[4] = q[4] * s
    return c

end

local function w_positive(q,o)

    if q[4] < 0 then
        scale(q,-1,o)
    else
        setq(o,q)
    end
    return o

end

local function invert(q,o)

    conjugate(q,o)
    local mag_sqr = len_sqr(o)
    if mag_sqr == 0 then return o end

    return scale(o, 1 / math.sqrt(mag_sqr), o)

end

local function get_perpendicular(q, o)

    o[1] = q[2]
    o[2] = q[1] * -1
    o[3] = q[4]
    o[4] = q[3] * -1
    return o

end

local function get_twist(q,axis,twist)

    twist = twist or new(0,0,0,1)

    local dot = kvec3.dot(q,axis)
    twist[1] = dot * axis[1]
    twist[2] = dot * axis[2]
    twist[3] = dot * axis[3]
    twist[4] = q[4]

    local l = len_sqr(twist)
    if l ~= 0 then
        return scale(twist, 1 / math.sqrt(l), twist)
    else
        return ident(twist)
    end

end

local function get_swing_twist(q,swing,twist)

    swing = swing or new(0,0,0,1)
    twist = twist or new(0,0,0,1)

    local x,y,z,w = q[1], q[2], q[3], q[4]
    local s = math.sqrt(w ^ 2 + x ^ 2)
    if s ~= 0 then
        set(twist, x / s, 0, 0, w / s)
        set(swing, 0, (w * y - x * z) / s, (w * z + x * y) / s, s)
    else
        ident(twist)
        setq(swing, q)
    end

    return swing, twist

end

local function get_axis_x(q, axis)

    local x,y,z,w = q[1], q[2], q[3], q[4]
    local tx, tw = 2 * x, 2 * w
    set(axis, tx * x + tw * w - 1, tx * y + z * tw, tx * z - y * tw)
    return axis

end

local function get_axis_y(q, axis)

    local x,y,z,w = q[1], q[2], q[3], q[4]
    local ty, tw = 2 * y, 2 * w
    set(axis, x * ty - z * tw, tw * w + ty * y - 1, x * tw + ty * z)
    return axis

end

local function get_axis_z(q, axis)

    local x,y,z,w = q[1], q[2], q[3], q[4]
    local tz, tw = 2 * z, 2 * w
    set(axis, x * tz + y * tw, y * tz - x * tw, tw * w + tz * z - 1)
    return axis

end

local function get_angle(q, axis)

    return q[4] == 0 and math.pi or (2 * math.atan( kvec3.dot(q, axis) / q[4] ))

end

local function compare(a,b,t)

    if t then
        return math.abs(a[1] - b[1]) < t
        and    math.abs(a[2] - b[2]) < t
        and    math.abs(a[3] - b[3]) < t
        and    math.abs(a[4] - b[4]) < t
    end
    return a[1] == b[1] and a[2] == b[2] and a[3] == b[3] and a[4] == b[4]

end

local function random(q)

    local x = math.random()
    local p0 = math.random() * math.pi * 2
    local p1 = math.random() * math.pi * 2
    local r1 = math.sqrt(1 - x)
    local r2 = math.sqrt(x)
    local s0 = math.sin(p0)
    local c0 = math.cos(p0)
    local s1 = math.sin(p1)
    local c1 = math.cos(p1)
    q[1] = s0 * r1
    q[2] = c0 * r1
    q[3] = s1 * r2
    q[4] = c1 * r2
    return q

end

local rot_u = kvec3.new()
local rot_w = kvec3.new()
local function rotate_vec(q, vec, out)

    rot_u[1], rot_u[2], rot_u[3] = q[1], q[2], q[3]
    local s = q[4]

    local d0 = kvec3.dot(rot_u, vec)
    local d1 = kvec3.dot(rot_u, rot_u)
    kvec3.cross(rot_u, vec, rot_w)

    kvec3.setv(out, kvec3.zero)
    kvec3.ma(out, rot_u, d0 * 2, out)
    kvec3.ma(out, vec, (s*s - d1), out)
    kvec3.ma(out, rot_w, 2 * s, out)
    return out

end

local function from_axis_angle(q, axis, angle)

    q = q or new()

    angle = angle * 0.5
    local s = math.sin(angle)
    set(q, axis[1] * s, axis[2] * s, axis[3] * s, math.cos(angle))
    return q

end

local function to_axis_angle(q, out_axis)

    local out_angle = 0
    out_axis = out_axis or kvec3.new()
    w_positive(q, tq)
    if tq[4] >= 1.0 then
        kvec3.ident(out_axis)
        out_angle = 0
    else
        kvec3.normalized_or_zero( tq, out_axis )
        out_angle = 2 * math.acos(tq[4])
    end
    return out_axis, out_angle

end

local function from_euler(q,v)

    q = q or new()
    local x,y,z = v[1]*.5, v[2]*.5, v[3]*.5

    local cx = math.cos(x)
    local sx = math.sin(x)
    local cy = math.cos(y)
    local sy = math.sin(y)
    local cz = math.cos(z)
    local sz = math.sin(z)

    q[1] = cz * sx * cy - sz * cx * sy
    q[2] = cz * cx * sy + sz * sx * cy
    q[3] = sz * cx * cy - cz * sx * sy
    q[4] = cz * cx * cy + sz * sx * sy
    return q

end

local function to_euler(q,v)

    v = v or kvec3.new()

    local y_sq = q[2] ^ 2

    local t0 = 2 * (q[4] * q[1] + q[2] * q[3])
    local t1 = 1 - 2 * (q[1] ^ 2 + y_sq)

    local t2 = 2 * (q[4] * q[2] - q[3] * q[1])
    t2 = t2 > 1.0 and 1.0 or t2
    t2 = t2 < -1.0 and -1.0 or t2

    local t3 = 2 * (q[4] * q[3] + q[1] * q[2])
    local t4 = 1 - 2 * (y_sq + q[3] ^ 2)

    v[1] = math.atan2(t0, t1)
    v[2] = math.asin(t2)
    v[3] = math.atan2(t3, t4)
    return v

end

local tv = {}
local function from_gmod(q,a)

    q = q or new()
    kvec3.from_gmod(tv, a)
    kvec3.scale(tv, math.pi / 180, tv)
    from_euler(q, tv)
    return q

end

local function to_gmod(q,a)

    if not a then
        to_euler(q, tv)
        kvec3.scale(tv, 180 / math.pi, tv)
        a = Angle(unpack(tv))
    else
        to_euler(q, tv)
        kvec3.scale(tv, 180 / math.pi, tv)
        a:SetUnpacked(unpack(tv))
    end
    return a

end

return {
    new = new,
    ident = ident,
    set = set,
    setq = setq,
    add = add,
    sub = sub,
    dot = dot,
    conjugate = conjugate,
    mul = mul,
    mul_vec = mul_vec,
    len_sqr = len_sqr,
    len = len,
    normalized = normalized,
    is_normalized = is_normalized,
    scale = scale,
    w_positive = w_positive,
    invert = invert,
    get_perpendicular = get_perpendicular,
    get_twist = get_twist,
    get_swing_twist = get_swing_twist,
    get_axis_x = get_axis_x,
    get_axis_y = get_axis_y,
    get_axis_z = get_axis_z,
    get_angle = get_angle,
    compare = compare,
    random = random,
    rotate_vec = rotate_vec,
    from_axis_angle = from_axis_angle,
    to_axis_angle = to_axis_angle,
    from_euler = from_euler,
    to_euler = to_euler,
    from_gmod = from_gmod,
    to_gmod = to_gmod,
}