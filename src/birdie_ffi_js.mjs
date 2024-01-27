import { Error, Ok } from "./gleam.mjs";

export function terminal_width() {
    // Node
    try {
        const width = process.stdout.columns;
        if (width) return new Ok(width);
    } catch { }

    // Deno
    try {
        const { columns: width, rows: _ } = Deno.consoleSize();
        return new Ok(width);
    } catch { }

    return new Error(undefined);
}
