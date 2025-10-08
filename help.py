import os
import shutil
import sys

SDK_PATH = r"C:\Program Files (x86)\Windows Kits\10\Lib\10.0.22621.0"
TARGET_LIBS = ["msvcrt.lib", "kernel32.lib", "legacy_stdio_definitions.lib"]
ARCHS = ["x64", "x86"]
DEST_ROOT = os.path.join(os.getcwd(), "src", "libs")

for arch in ARCHS:
    dest_dir = os.path.join(DEST_ROOT, arch)
    os.makedirs(dest_dir, exist_ok=True)

    for lib in TARGET_LIBS:
        found = False
        for folder in ["um", "ucrt"]:
            src_path = os.path.join(SDK_PATH, folder, arch, lib)
            if os.path.exists(src_path):
                dst_path = os.path.join(dest_dir, lib)
                shutil.copy2(src_path, dst_path)
                print(f"Copied {src_path} -> {dst_path}")
                found = True
                break
        if not found:
            print(f"Library not found: {lib} for {arch}")

print("Done! Both x64 and x86 libraries copied.")
