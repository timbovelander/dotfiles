atom.commands.add 'atom-text-editor', 'pane:switch-next-recently-used-item', ->
  pane = atom.workspace.getActivePane()
  pane.activateNextRecentlyUsedItem()
  pane.moveActiveItemToTopOfStack()
