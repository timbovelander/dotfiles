atom.commands.add 'atom-text-editor', 'pane:switch-next-recently-used-item', ->
  pane = atom.workspace.getActivePane()
  pane.activateNextRecentlyUsedItem()
  pane.moveActiveItemToTopOfStack()

atom.commands.add '.tree-view', 'tree-view:open-and-toggle-tree-view', (ev) ->
  atom.commands.dispatch(ev.currentTarget, 'tree-view:open-selected-entry')
  if ev.currentTarget.querySelector('.selected').getAttribute('is') is 'tree-view-file'
    atom.commands.dispatch(ev.currentTarget, 'tree-view:toggle')
