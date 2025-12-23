import fs from "node:fs";
import { exit } from "node:process";
import { Result$Ok, Result$Error } from "./gleam.mjs";

export function is_windows() {
  return globalThis?.process?.platform === "win32" || globalThis?.Deno?.build?.os === "windows";
}

export function halt(status_code) {
  exit(status_code);
}

export function get_line(prompt) {
  process.stdout.write(prompt);
  const buffer = Buffer.alloc(4096);
  while (true) {
    try {
      const bytesRead = fs.readSync(process.stdin.fd, buffer, 0, buffer.length, null);
      const input = buffer.toString("utf-8", 0, bytesRead);
      return Result$Ok(input);
    } catch (error) {
      if (error.code === "EAGAIN") continue;
      return Result$Error(undefined);
    }
  }
}
