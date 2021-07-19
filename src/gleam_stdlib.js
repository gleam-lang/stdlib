export function identity(x) {
    return x
}

export function parse_int(value) {
    if (/^[-+]?(\d+)$/.test(value)) {
        return { "type": "Ok", "0": Number(value) }
    } else {
        return { "type": "Error", "0": null }
    }
}

export function int_to_string(int) {
    return int.toString()
}

export function int_to_base_string(int, base) {
    return int.toString(base)
}

export function parse_float(value) {
    if (/^[-+]?(\d+.\d*)$/.test(value)) {
        return { "type": "Ok", "0": Number(value) }
    } else {
        return { "type": "Error", "0": null }
    }
}

export function float_to_string(float) {
    let number = float.toString()
    return number.includes(".") ? number : number.concat(".0")
}

export function ceiling(float) {
    return Math.ceil(float)
}

export function floor(float) {
    return Math.floor(float)
}

export function round(float) {
    return Math.round(float)
}

export function truncate(float) {
    return Math.trunc(float)
}

export function absolute_value(float) {
    return Math.abs(float)
}

export function power(x, y) {
    return Math.pow(x, y)
}