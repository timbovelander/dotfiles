fs.openSync("#{process.env.ATOM_HOME}/scratch", 'w')

atom.commands.add 'atom-text-editor', 'editor:copy-all', ->
  editor = atom.workspace.getActiveTextEditor()
  editor.selectAll()
  editor.copySelectedText()

atom.commands.add 'atom-text-editor', 'editor:delete-all', ->
  editor = atom.workspace.getActiveTextEditor()
  editor.selectAll()
  editor.delete()

atom.commands.add 'atom-text-editor', 'editor:scratch', ->
  atom.workspace.open("#{process.env.ATOM_HOME}/scratch")

atom.commands.add 'atom-text-editor', 'pane:switch-next-recently-used-item', ->
  pane = atom.workspace.getActivePane()
  pane.activateNextRecentlyUsedItem()
  pane.moveActiveItemToTopOfStack()

atom.commands.add '.tree-view', 'tree-view:open-selected-entry-and-toggle', (ev) ->
  atom.commands.dispatch(ev.currentTarget, 'tree-view:open-selected-entry')
  if ev.currentTarget.querySelector('.selected').getAttribute('is') is 'tree-view-file'
    atom.commands.dispatch(ev.currentTarget, 'tree-view:toggle')
