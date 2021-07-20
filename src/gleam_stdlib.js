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