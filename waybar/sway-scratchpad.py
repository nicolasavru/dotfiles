#!/usr/bin/python
from collections.abc import Sequence
import json

import i3ipc


def scratchpad_containers(i3: i3ipc.Connection) -> list[i3ipc.Con]:
  # scratchpad_workspace = i3.get_tree().find_named("__i3_scratch")
  scratchpad = i3.get_tree().scratchpad()
  return (scratchpad.floating_nodes
          if scratchpad
          else [])


def format_output(scratchpad_containers: Sequence[i3ipc.con]) -> str:
  return json.dumps({
    "text": str(len(scratchpad_containers)),
    "class": ("scratchpad-populated"
              if scratchpad_containers
              else "scratchpad-empty"),
    "tooltip": "\n".join(container.name for container in scratchpad_containers),
  })


def write_output(output: str) -> None:
  print(output, flush=True)


if __name__ == "__main__":
  write_output(format_output([]))
  i3 = i3ipc.Connection()
  i3.on(i3ipc.Event.WINDOW_MOVE,
        lambda i3, event: write_output(format_output(scratchpad_containers(i3))))
  i3.main()
