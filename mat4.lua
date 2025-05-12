AddCSLuaFile()

local kvec3 = include "vec3.lua"
local kquat = include "quat.lua"

local mt = {}
local fmt = "[%0.2f, %0.2f, %0.2f, %0.2f]"
fmt = table.concat({fmt,fmt,fmt,fmt}, "\n")
function mt.__tostring(m)
    return fmt:format(
    m[1 ], m[2 ], m[3 ], m[4 ],
    m[5 ], m[6 ], m[7 ], m[8 ],
    m[9 ], m[10], m[11], m[12],
    m[13], m[14], m[15], m[16])
end

local function new(...)
    local a = select(1, ...)
    if istable(a) then
        local t = {}
        for i=1, 16 do t[i] = a[i] end
        return setmetatable(t, mt)
    elseif a ~= nil then
        local t = {}
        for i=1, 16 do t[i] = select(i, ...) or 0 end
        return setmetatable(t, mt)
    else
        return setmetatable({
            1, 0, 0, 0,
            0, 1, 0, 0,
            0, 0, 1, 0,
            0, 0, 0, 1,
        }, mt)
    end
end

local function setm(m, o)

    for i=1, 16 do m[i] = o[i] end
    return m

end

local function get_at(m, r, c)

    return m[1 + c + (r * 4)]

end

local function set_at(m, r, c, v)

    m[1 + c + (r * 4)] = v
    return m

end

local function set_column3(m, i, v)

    m[1 + i] = v[1]
    m[5 + i] = v[2]
    m[9 + i] = v[3]
    return m

end

local function get_column3(m, i, v)

    v[1] = m[1 + i]
    v[2] = m[5 + i]
    v[3] = m[9 + i]
    return v

end

local function set_column4(m, i, v)

    m[1 + i] = v[1]
    m[5 + i] = v[2]
    m[9 + i] = v[3]
    m[13 + i] = v[4]
    return m

end

local function get_column4(m, i, v)

    v[1] = m[1 + i]
    v[2] = m[5 + i]
    v[3] = m[9 + i]
    v[4] = m[13 + i]
    return v

end

local function get_axis_x(m, v) return get_column3(m, 0, v) end
local function get_axis_y(m, v) return get_column3(m, 1, v) end
local function get_axis_z(m, v) return get_column3(m, 2, v) end

local function set_axis_x(m, v) return set_column3(m, 0, v) end
local function set_axis_y(m, v) return set_column3(m, 1, v) end
local function set_axis_z(m, v) return set_column3(m, 2, v) end

local function get_rotation(m, o)

    o[ 1], o[ 2], o[ 3], o[ 4] = m[ 1], m[ 2], m[ 3], 0
    o[ 5], o[ 6], o[ 7], o[ 8] = m[ 5], m[ 6], m[ 7], 0
    o[ 9], o[10], o[11], o[12] = m[ 9], m[10], m[11], 0
    o[13], o[14], o[15], o[16] = 0, 0, 0, 1
    return o

end

local function get_transposed4(m, o)

    o[ 1], o[ 2], o[ 3], o[ 4],
    o[ 5], o[ 6], o[ 7], o[ 8],
    o[ 9], o[10], o[11], o[12],
    o[13], o[14], o[15], o[16] =
    m[ 1], m[ 5], m[ 9], m[13],
    m[ 2], m[ 6], m[10], m[14],
    m[ 3], m[ 7], m[11], m[15],
    m[ 4], m[ 8], m[12], m[16]
    return o

end

local function get_transposed3(m, o)

    o[ 1], o[ 2], o[ 3],
    o[ 5], o[ 6], o[ 7],
    o[ 9], o[10], o[11] =
    m[ 1], m[ 5], m[ 9],
    m[ 2], m[ 6], m[10],
    m[ 3], m[ 7], m[11]
    return o

end


local function ident(m)

    m[ 1], m[ 2], m[ 3], m[ 4] = 1, 0, 0, 0
    m[ 5], m[ 6], m[ 7], m[ 8] = 0, 1, 0, 0
    m[ 9], m[10], m[11], m[12] = 0, 0, 1, 0
    m[13], m[14], m[15], m[16] = 0, 0, 0, 1
    return m

end

local function zero(m)

    m[ 1], m[ 2], m[ 3], m[ 4] = 0, 0, 0, 0
    m[ 5], m[ 6], m[ 7], m[ 8] = 0, 0, 0, 0
    m[ 9], m[10], m[11], m[12] = 0, 0, 0, 0
    m[13], m[14], m[15], m[16] = 0, 0, 0, 0
    return m

end

local function add(a, b, c)

    c[ 1], c[ 2], c[ 3], c[ 4],
    c[ 5], c[ 6], c[ 7], c[ 8],
    c[ 9], c[10], c[11], c[12],
    c[13], c[14], c[15], c[16] =
    a[ 1] + b[ 1], a[ 2] + b[ 2], a[ 3] + b[ 3], a[ 4] + b[ 4],
    a[ 5] + b[ 5], a[ 6] + b[ 6], a[ 7] + b[ 7], a[ 8] + b[ 8],
    a[ 9] + b[ 9], a[10] + b[10], a[11] + b[11], a[12] + b[12],
    a[13] + b[13], a[14] + b[14], a[15] + b[15], a[16] + b[16]
    return c

end

local function mul(a, s, c)

    c[ 1], c[ 2], c[ 3], c[ 4],
    c[ 5], c[ 6], c[ 7], c[ 8],
    c[ 9], c[10], c[11], c[12],
    c[13], c[14], c[15], c[16] =
    a[ 1] * s, a[ 2] * s, a[ 3] * s, a[ 4] * s,
    a[ 5] * s, a[ 6] * s, a[ 7] * s, a[ 8] * s,
    a[ 9] * s, a[10] * s, a[11] * s, a[12] * s,
    a[13] * s, a[14] * s, a[15] * s, a[16] * s
    return c

end

local function mul_vec3(m, v, o)

    o[1], o[2], o[3] = 
    m[ 1] * v[1] + m[ 2] * v[2] + m[ 3] * v[3],
    m[ 5] * v[1] + m[ 6] * v[2] + m[ 7] * v[3],
    m[ 9] * v[1] + m[10] * v[2] + m[11] * v[3]

    return o

end

local function mul_vec3w(m, v, o)

    o[1], o[2], o[3] = 
    m[ 1] * v[1] + m[ 2] * v[2] + m[ 3] * v[3] + m[ 4],
    m[ 5] * v[1] + m[ 6] * v[2] + m[ 7] * v[3] + m[ 8],
    m[ 9] * v[1] + m[10] * v[2] + m[11] * v[3] + m[12]

    return o

end

local function mul_vec(m, v, w, o)

    o[1], o[2], o[3] = 
    m[ 1] * v[1] + m[ 2] * v[2] + m[ 3] * v[3] + m[ 4] * w,
    m[ 5] * v[1] + m[ 6] * v[2] + m[ 7] * v[3] + m[ 8] * w,
    m[ 9] * v[1] + m[10] * v[2] + m[11] * v[3] + m[12] * w

    return o

end

local function mul_vec_transposed(m, v, w, o)

    o[1], o[2], o[3] =
    m[ 1] * v[1] + m[ 5] * v[2] + m[ 9] * v[3] + m[13] * w,
    m[ 2] * v[1] + m[ 6] * v[2] + m[10] * v[3] + m[14] * w,
    m[ 3] * v[1] + m[ 7] * v[2] + m[11] * v[3] + m[15] * w

    return o

end

local function mul_vec_transposed3(m, v, o)

    o[1], o[2], o[3] =
    m[ 1] * v[1] + m[ 5] * v[2] + m[ 9] * v[3],
    m[ 2] * v[1] + m[ 6] * v[2] + m[10] * v[3],
    m[ 3] * v[1] + m[ 7] * v[2] + m[11] * v[3]

    return o

end

local function translation(m, v)

    m[ 1], m[ 2], m[ 3], m[ 4] = 1, 0, 0, v[1]
    m[ 5], m[ 6], m[ 7], m[ 8] = 0, 1, 0, v[2]
    m[ 9], m[10], m[11], m[12] = 0, 0, 1, v[3]
    m[13], m[14], m[15], m[16] = 0, 0, 0, 1
    return m

end

local function scale(m, v)

    m[ 1], m[ 2], m[ 3], m[ 4] = v[1], 0, 0, 0
    m[ 5], m[ 6], m[ 7], m[ 8] = 0, v[2], 0, 0
    m[ 9], m[10], m[11], m[12] = 0, 0, v[3], 0
    m[13], m[14], m[15], m[16] = 0, 0, 0, 1
    return m

end

local function rotation_x(m, x)

    local c = math.cos(x)
    local s = math.sin(x)
    m[ 1], m[ 2], m[ 3], m[ 4] = 1, 0, 0, 0
    m[ 5], m[ 6], m[ 7], m[ 8] = 0, c, -s, 0
    m[ 9], m[10], m[11], m[12] = 0, s, c, 0
    m[13], m[14], m[15], m[16] = 0, 0, 0, 1
    return m

end

local function rotation_y(m, y)

    local c = math.cos(y)
    local s = math.sin(y)
    m[ 1], m[ 2], m[ 3], m[ 4] = c, 0, s, 0
    m[ 5], m[ 6], m[ 7], m[ 8] = 0, 1, 0, 0
    m[ 9], m[10], m[11], m[12] = -s,0, c, 0
    m[13], m[14], m[15], m[16] = 0, 0, 0, 1
    return m

end

local function rotation_z(m, z)

    local c = math.cos(z)
    local s = math.sin(z)
    m[ 1], m[ 2], m[ 3], m[ 4] = c, -s, 0, 0
    m[ 5], m[ 6], m[ 7], m[ 8] = s, c, 0, 0
    m[ 9], m[10], m[11], m[12] = 0, 0, 1, 0
    m[13], m[14], m[15], m[16] = 0, 0, 0, 1
    return m

end

local function rotation_quat(m, q)

    local x,y,z,w = q[1],q[2],q[3],q[4]

    local tx = x + x
    local ty = y + y
    local tz = z + z

    local xx = tx * x
    local yy = ty * y
    local zz = tz * z
    local xy = tx * y
    local xz = tx * z
    local xw = tx * w
    local yz = ty * z
    local yw = ty * w
    local zw = tz * w

    m[ 1], m[ 5], m[ 9], m[13] = (1 - yy) - zz, xy + zw, xz - yw, 0
    m[ 2], m[ 6], m[10], m[14] = xy - zw, (1 - zz) - xx, yz + xw, 0
    m[ 3], m[ 7], m[11], m[15] = xz + yw, yz - xw, (1 - xx) - yy, 0
    m[ 4], m[ 8], m[12], m[16] = 0, 0, 0, 1
    return m

end

local tq = {}
local function rotation_axis(m, axis, angle)

    kquat.from_axis_angle(tq, axis, angle)
    rotation_quat(m, tq)
    return m

end

local function set_translation(m, v)

    m[ 4] = v[1]
    m[ 8] = v[2]
    m[12] = v[3]
    m[16] = 1
    return m

end

local function rotation_translation(m, q, v)

    rotation_quat(m, q)
    set_translation(m, v)
    return m

end

local q_inv = {}
local v_inv = {}
local function inv_rotation_translation(m, q, v)

    kquat.conjugate(q, q_inv)
    rotation_quat(m, q_inv)
    mul_vec(m, v, 0, v_inv)
    kvec3.scale(v_inv, -1, v_inv)
    set_translation(m, v_inv)
    return m

end

local function outer_product(m, v1, v2)

    local x,y,z = v2[1], v2[2], v2[3]
    m[ 1], m[ 2], m[ 3], m[ 4] = v1[1] * x, v1[1] * y, v1[1] * z, 0
    m[ 5], m[ 6], m[ 7], m[ 8] = v1[2] * x, v1[2] * y, v1[2] * z, 0
    m[ 9], m[10], m[11], m[12] = v1[3] * x, v1[3] * y, v1[3] * z, 0
    m[13], m[14], m[15], m[16] = 0, 0, 0, 1
    return m

end

local function cross_product(m, v)

    local x,y,z = v[1],v[2],v[3]
    m[ 1], m[ 2], m[ 3], m[ 4] = 0, -z, y, 0
    m[ 5], m[ 6], m[ 7], m[ 8] = z, 0, -x, 0
    m[ 9], m[10], m[11], m[12] = -y, x, 0, 0
    m[13], m[14], m[15], m[16] = 0, 0, 0, 1
    return m

end

local function concat3(a, b, c)

    for i=0, 8, 4 do
        c[1+i] = a[1+i] * b[1 ] + a[2+i] * b[5 ] + a[3+i] * b[9 ]
        c[2+i] = a[1+i] * b[2 ] + a[2+i] * b[6 ] + a[3+i] * b[10]
        c[3+i] = a[1+i] * b[3 ] + a[2+i] * b[7 ] + a[3+i] * b[11]
    end
    return c

end

local function concat4(a, b, c)

    for i=0, 12, 4 do
        c[1+i] = a[1+i] * b[1] + a[2+i] * b[5] + a[3+i] * b[9 ] + a[4+i] * b[13]
        c[2+i] = a[1+i] * b[2] + a[2+i] * b[6] + a[3+i] * b[10] + a[4+i] * b[14]
        c[3+i] = a[1+i] * b[3] + a[2+i] * b[7] + a[3+i] * b[11] + a[4+i] * b[15]
        c[4+i] = a[1+i] * b[4] + a[2+i] * b[8] + a[3+i] * b[12] + a[4+i] * b[16]
    end
    return c

end

local function concat3_left_transposed(a, b, c)

    local w = 0
    for i=0,2 do
        c[1+w] = a[1+i] * b[1] + a[5+i] * b[5] + a[9+i] * b[9 ]
        c[2+w] = a[1+i] * b[2] + a[5+i] * b[6] + a[9+i] * b[10]
        c[3+w] = a[1+i] * b[3] + a[5+i] * b[7] + a[9+i] * b[11]
        w = w + 4
    end

    c[4], c[8], c[12], c[16] = 0, 0, 0, 1
    return c

end

local function concat3_right_transposed(a, b, c)

    for i=0, 8, 4 do
        c[1+i] = a[1+i] * b[1 ] + a[2+i] * b[2 ] + a[3+i] * b[3 ]
        c[2+i] = a[1+i] * b[5 ] + a[2+i] * b[6 ] + a[3+i] * b[7 ]
        c[3+i] = a[1+i] * b[9 ] + a[2+i] * b[10] + a[3+i] * b[11]
    end

    c[4], c[8], c[12], c[16] = 0, 0, 0, 1
    return c

end

local function compare(a,b,t)

    if not t then
        for i=1, 16 do
            if a[i] ~= b[i] then return false end
        end
    else
        for i=1, 16 do
            if math.abs(a[i] - b[i]) > t then return false end
        end
    end
    return true

end

local function determinant3(m)

    return 
        m[ 1] * (m[ 6] * m[11] - m[10] * m[ 7]) + 
        m[ 5] * (m[10] * m[ 3] - m[ 2] * m[11]) + 
        m[ 9] * (m[ 2] * m[ 7] - m[ 6] * m[ 3])

end

local function determinant4(m)

	local a0 = m[ 1] * m[ 6] - m[ 2] * m[ 5]
	local a1 = m[ 1] * m[ 7] - m[ 3] * m[ 5]
	local a2 = m[ 1] * m[ 8] - m[ 4] * m[ 5]
	local a3 = m[ 2] * m[ 7] - m[ 3] * m[ 6]
	local a4 = m[ 2] * m[ 8] - m[ 4] * m[ 6]
	local a5 = m[ 3] * m[ 9] - m[ 4] * m[ 7]
	local b0 = m[ 9] * m[14] - m[10] * m[13]
	local b1 = m[ 9] * m[15] - m[11] * m[13]
	local b2 = m[ 9] * m[16] - m[12] * m[13]
	local b3 = m[10] * m[15] - m[11] * m[14]
	local b4 = m[10] * m[16] - m[12] * m[14]
	local b5 = m[11] * m[16] - m[12] * m[15]
	return a0 * b5 - a1 * b4 + a2 * b3 + a3 * b2 - a4 * b1 + a5 * b0

end

local function invert3(m, o)

    local det = determinant3(m)
    local invdet = 1/det

    o[ 1], o[ 5], o[ 9], o[13],
    o[ 2], o[ 6], o[10], o[14],
    o[ 3], o[ 7], o[11], o[15],
    o[ 4], o[ 8], o[12], o[16] =
    (m[ 6] * m[11] - m[ 7] * m[10]) * invdet,
    (m[ 7] * m[ 9] - m[ 5] * m[11]) * invdet,
    (m[ 5] * m[10] - m[ 6] * m[ 9]) * invdet,
    0,
    (m[ 3] * m[10] - m[ 2] * m[11]) * invdet,
    (m[ 1] * m[11] - m[ 3] * m[ 9]) * invdet,
    (m[ 2] * m[ 9] - m[ 1] * m[10]) * invdet,
    0,
    (m[ 2] * m[ 7] - m[ 3] * m[ 6]) * invdet,
    (m[ 3] * m[ 5] - m[ 1] * m[ 7]) * invdet,
    (m[ 1] * m[ 6] - m[ 2] * m[ 5]) * invdet,
    0,
    0, 0, 0, 1

    return o

end

local function invert4(m, o)

    local a0 = m[ 1] * m[ 6] - m[ 2] * m[ 5]
	local a1 = m[ 1] * m[ 7] - m[ 3] * m[ 5]
	local a2 = m[ 1] * m[ 8] - m[ 4] * m[ 5]
	local a3 = m[ 2] * m[ 7] - m[ 3] * m[ 6]
	local a4 = m[ 2] * m[ 8] - m[ 4] * m[ 6]
	local a5 = m[ 3] * m[ 8] - m[ 4] * m[ 7]
	local b0 = m[ 9] * m[14] - m[10] * m[13]
	local b1 = m[ 9] * m[15] - m[11] * m[13]
	local b2 = m[ 9] * m[16] - m[12] * m[13]
	local b3 = m[10] * m[15] - m[11] * m[14]
	local b4 = m[10] * m[16] - m[12] * m[14]
	local b5 = m[11] * m[16] - m[12] * m[15]
	local det = a0 * b5 - a1 * b4 + a2 * b3 + a3 * b2 - a4 * b1 + a5 * b0
    local invdet = 1/det

    o[ 1], o[ 5], o[ 9], o[13],
    o[ 2], o[ 6], o[10], o[14],
    o[ 3], o[ 7], o[11], o[15],
    o[ 4], o[ 8], o[12], o[16] =
    ( m[ 6]*b5 - m[ 7]*b4 + m[ 8]*b3) * invdet,
    (-m[ 5]*b5 + m[ 7]*b2 - m[ 8]*b1) * invdet,
    ( m[ 5]*b4 - m[ 6]*b2 + m[ 8]*b0) * invdet,
    (-m[ 5]*b3 + m[ 6]*b1 - m[ 7]*b0) * invdet,
    (-m[ 2]*b5 + m[ 3]*b4 - m[ 4]*b3) * invdet,
    ( m[ 1]*b5 - m[ 3]*b2 + m[ 4]*b1) * invdet,
    (-m[ 1]*b4 + m[ 2]*b2 - m[ 4]*b0) * invdet,
    ( m[ 1]*b3 - m[ 2]*b1 + m[ 3]*b0) * invdet,
    ( m[14]*a5 - m[15]*a4 + m[16]*a3) * invdet,
    (-m[13]*a5 + m[15]*a2 - m[16]*a1) * invdet,
    ( m[13]*a4 - m[14]*a2 + m[16]*a0) * invdet,
    (-m[13]*a3 + m[14]*a1 - m[15]*a0) * invdet,
    (-m[10]*a5 + m[11]*a4 - m[12]*a3) * invdet,
    ( m[ 9]*a5 - m[11]*a2 + m[12]*a1) * invdet,
    (-m[ 9]*a4 + m[10]*a2 - m[12]*a0) * invdet,
    ( m[ 9]*a3 - m[10]*a1 + m[11]*a0) * invdet

    return o

end

local tv_pt = {}
local function pre_translated(m, v, o)

    local pt = mul_vec(m, v, 0, tv_pt)

    if m ~= o then
        o[ 1] = m[ 1]
        o[ 5] = m[ 5]
        o[ 9] = m[ 9]
        o[13] = m[13]
        o[ 2] = m[ 2]
        o[ 6] = m[ 6]
        o[10] = m[10]
        o[14] = m[14]
        o[ 3] = m[ 3]
        o[ 7] = m[ 7]
        o[11] = m[11]
        o[15] = m[15]
    end
    o[ 4] = m[ 4] + pt[1]
    o[ 8] = m[ 8] + pt[2]
    o[12] = m[12] + pt[3]
    o[16] = 1
    return o

end

local function post_translated(m, v, o)

    if m ~= o then
        o[ 1] = m[ 1]
        o[ 5] = m[ 5]
        o[ 9] = m[ 9]
        o[13] = m[13]
        o[ 2] = m[ 2]
        o[ 6] = m[ 6]
        o[10] = m[10]
        o[14] = m[14]
        o[ 3] = m[ 3]
        o[ 7] = m[ 7]
        o[11] = m[11]
        o[15] = m[15]
    end
    o[ 4] = m[ 4] + v[1]
    o[ 8] = m[ 8] + v[2]
    o[12] = m[12] + v[3]
    o[16] = 1
    return o

end

local __tmp0 = kvec3.new()
local __tmp1 = kvec3.new()
local __tmp2 = kvec3.new()
local __tmp3 = kvec3.new()
local function rotated_aabb(m, min, max, new_min, new_max)

    kvec3.set(new_min, m[4], m[8], m[12])
    kvec3.set(new_max, m[4], m[8], m[12])

    for i=0, 2 do
        kmat4.get_column3(m, i, __tmp0)

        local a = kvec3.scale(__tmp0, min[i+1], __tmp1)
        local b = kvec3.scale(__tmp0, max[i+1], __tmp2)

        kvec3.add(new_min, kvec3.min(a,b,__tmp3), new_min)
        kvec3.add(new_max, kvec3.max(a,b,__tmp3), new_max)
    end

    return new_min, new_max

end

local idx = {1,6,11}
local function to_quat(m, q)

    --[[
    
      0      1      2      3
    0 m[ 1], m[ 2], m[ 3], m[ 4]
    1 m[ 5], m[ 6], m[ 7], m[ 8]
    2 m[ 9], m[10], m[11], m[12]
    3 m[13], m[14], m[15], m[16]

    ]]

    local tr = m[1] + m[6] + m[11]
    if tr >= 0 then
        local s = math.sqrt(1+tr)
        local is = 0.5/s
        q[1] = (m[10] - m[ 7]) * is
        q[2] = (m[ 3] - m[ 9]) * is
        q[3] = (m[ 5] - m[ 2]) * is
        q[4] = 0.5 * s
        return q
    else
        local i = 1
        if m[ 6] > m[     1] then i = 2 end
        if m[11] > m[idx[i]] then i = 3 end
        if i == 1 then
            local s = math.sqrt(m[ 1] - (m[ 6] + m[11]) + 1)
            local is = 0.5/s
            q[1] = 0.5 * s
            q[2] = (m[ 2] + m[ 5]) * is
            q[3] = (m[ 9] + m[ 3]) * is
            q[4] = (m[10] - m[ 7]) * is
            return q
        elseif i == 2 then
            local s = math.sqrt(m[ 6] - (m[11] + m[ 1]) + 1)
            local is = 0.5/s
            q[1] = (m[ 2] + m[ 5]) * is
            q[2] = 0.5 * s
            q[3] = (m[ 7] + m[10]) * is
            q[4] = (m[ 3] - m[ 9]) * is
            return q
        else
            local s = math.sqrt(m[11] - (m[ 1] + m[ 6]) + 1)
            local is = 0.5/s
            q[1] = (m[ 9] + m[ 3]) * is
            q[2] = (m[ 7] + m[10]) * is
            q[3] = 0.5 * s
            q[4] = (m[ 5] - m[ 2]) * is
            return q
        end
    end

end

local function to_gmod(m, mtx)

    mtx = mtx or Matrix()
    mtx:SetUnpacked(
        m[ 1], m[ 2], m[ 3], m[ 4],
        m[ 5], m[ 6], m[ 7], m[ 8],
        m[ 9], m[10], m[11], m[12],
        m[13], m[14], m[15], m[16]
    )

    return mtx

end

local function from_gmod(m, mtx)

    m[ 1], m[ 2], m[ 3], m[ 4],
    m[ 5], m[ 6], m[ 7], m[ 8],
    m[ 9], m[10], m[11], m[12],
    m[13], m[14], m[15], m[16] = mtx:Unpack()

    return m

end

return {
    new = new,
    setm = setm,
    get_at = get_at,
    set_at = set_at,
    set_column3 = set_column3,
    get_column3 = get_column3,
    set_column4 = set_column4,
    get_column4 = get_column4,
    get_axis_x = get_axis_x,
    get_axis_y = get_axis_y,
    get_axis_z = get_axis_z,
    set_axis_x = set_axis_x,
    set_axis_y = set_axis_y,
    set_axis_z = set_axis_z,
    get_rotation = get_rotation,
    get_transposed3 = get_transposed3,
    get_transposed4 = get_transposed4,
    ident = ident,
    zero = zero,
    add = add,
    mul = mul,
    mul_vec = mul_vec,
    mul_vec3 = mul_vec3,
    mul_vec3w = mul_vec3w,
    mul_vec_transposed = mul_vec_transposed,
    mul_vec_transposed3 = mul_vec_transposed3,
    translation = translation,
    scale = scale,
    rotation_x = rotation_x,
    rotation_y = rotation_y,
    rotation_z = rotation_z,
    rotation_quat = rotation_quat,
    rotation_axis = rotation_axis,
    rotation_translation = rotation_translation,
    inv_rotation_translation = inv_rotation_translation,
    set_translation = set_translation,
    outer_product = outer_product,
    cross_product = cross_product,
    concat3 = concat3,
    concat4 = concat4,
    concat3_left_transposed = concat3_left_transposed,
    concat3_right_transposed = concat3_right_transposed,
    compare = compare,
    determinant3 = determinant3,
    determinant4 = determinant4,
    invert3 = invert3,
    invert4 = invert4,
    pre_translated = pre_translated,
    post_translated = post_translated,
    rotated_aabb = rotated_aabb,
    to_quat = to_quat,
    to_gmod = to_gmod,
    from_gmod = from_gmod,
}