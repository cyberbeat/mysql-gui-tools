
SimpleForm = {}
SimpleForm_mt = { __index = SimpleForm }

SF_LEFT="left"
SF_RIGHT="right"
SF_CENTER="center"

SFT_TEXT=0
SFT_PASSWORD=1
SFT_LIST=2
SFT_NUMBER=3

function SimpleForm:new()
  local inst = {}
  setmetatable(inst, SimpleForm_mt)
  return inst
end


function SimpleForm:init(title, width)
  self.title= title
  self.width= width
  self.form= nil
  self.height= 1
  self.objects= {}
  self.buttons= {}
  self.everything= {}
end

function SimpleForm:addSpace()
  self.height= self.height+1
end

function SimpleForm:addLabel(text, align)
  local label
  local x
  local y
  local txtlen

  y= self.height
  self.height= self.height + 1
  txtlen= string.len(text)

  if align == SF_LEFT then
    x= 1
  elseif align == SF_RIGHT then
    x= self.width-txtlen
  else
    x= (self.width-txtlen)/2
  end
  label= dlg.Label(x, y, text)

  table.insert(self.everything, label)
end


function SimpleForm:addCheck(name, text)
  local check

  check= dlg.Checkbox(2, self.height, text, 0)
  self.height= self.height+1

  table.insert(self.everything, check)
  self.objects[name]= {"check", check}
end


function SimpleForm:addEntries(forms)
  local name, caption, defval, etype
  local label, entry
  local x, y
  local leftWidth= 0
  local txtlen
  local row, _

  for _, row in forms do
    caption= row[2]

    if string.len(caption) > leftWidth then
      leftWidth= string.len(caption)
    end
  end

  for _, row in forms do
    name= row[1]
    caption= row[2]
    defval= row[3]
    etype= row[4]

    y= self.height
    self.height= self.height+1

    label= dlg.Label(1+leftWidth-string.len(caption), y, caption)

    if etype == SFT_TEXT then
      entry= dlg.Entry(leftWidth+1, y, self.width-leftWidth-2*1, defval, 0, 0)
      self.objects[name]= {"entry", entry}
    elseif etype == SFT_NUMBER then
      entry= dlg.Entry(leftWidth+1, y, 5, defval, 0, 0)
      self.objects[name]= {"entry", entry}
    elseif etype == SFT_PASSWORD then
      entry= dlg.Entry(leftWidth+1, y, self.width-leftWidth-2*1, defval, 0, 1)
      self.objects[name]= {"entry", entry}
    elseif etype == SFT_LIST then
      local item
      entry= dlg.Listbox(leftWidth+1, y, 1, 1, 1)
      for _, item in defval do
        entry:addItem(item)
      end
      self.objects[name]= {"listbox", entry}
    end
    table.insert(self.everything, label)
    table.insert(self.everything, entry)
  end
end


function SimpleForm:addListbox(name, options, visibleRows)
  local lbox
  local i
  local option

  lbox= dlg.Listbox(2, self.height, visibleRows, 1, 1)
  lbox:setWidth(self.width - 4)

  self.height= self.height + visibleRows

  for i, option in options do
    lbox:addItem(option)
  end

  self.objects[name]= {"listbox", lbox}
  table.insert(self.everything, lbox)
end


function SimpleForm:addCheckList(name, options, visibleRows)
  local lbox
  local i
  local option
  local oplist

  lbox= dlg.CheckboxTree(2, self.height, visibleRows, 1)
  lbox:setWidth(self.width - 4)

  self.height= self.height + visibleRows

  oplist= {}
  for i, option in options do
    local id= lbox:addItem(option[1], {}, option[2])
    table.insert(oplist, {option[3], id})
  end

  self.objects[name]= {"checklist", lbox, oplist}
  table.insert(self.everything, lbox)
end


function SimpleForm:addTreeView(options, visibleRows)
  local lbox
  local i, j, opti, optj

  lbox= dlg.CheckboxTree(2, self.height, visibleRows, 1, 1, 1)
  lbox:setWidth(self.width-4)

  for i, opti in options do
    lbox:addItem(opti[1], {dlg.ARG_APPEND}, 0)
    for j, optj in opti[2] do
      lbox:addItem(optj, {i-1, dlg.ARG_APPEND}, 0)
    end
  end
  table.insert(self.everything, lbox)

  self.height= self.height + visibleRows
end


function SimpleForm:addCheckTree(name, options, visibleRows)
  local lbox
  local i, j, opti, optj
  local itemlist= {}

  lbox= dlg.CheckboxTree(2, self.height, visibleRows, 1, 0, 0)
  lbox:setWidth(self.width-4)

  for i, opti in options do
    lbox:addItem(opti[1], {dlg.ARG_APPEND}, 0)
    for j, optj in opti[2] do
      local id= lbox:addItem(optj[1], {i-1, dlg.ARG_APPEND}, 0)
      table.insert(itemlist, {optj[2], id})
    end
  end
  table.insert(self.everything, lbox)

  self.height= self.height + visibleRows

  self.objects[name]= {"checktree", lbox, itemlist}
end


function SimpleForm:addWidget(w)
  table.insert(self.everything, w)
end

function SimpleForm:setButtons(lbuttons, rbuttons)
  local name, caption
  local width, b
  local button
  local x, y, _

  y= self.height
  self.height= self.height + 4

  x= 1
  for _, b in lbuttons do
    name= b[1]
    caption= b[2]

    button= dlg.Button(x, y, caption)
    self.buttons[name]= button
    table.insert(self.everything, button)

    x= x+string.len(caption)+5
  end

  x= self.width
  for _, b in rbuttons do
    x=x - (string.len(b[2])+5)
  end
  for _, b in rbuttons do
    name= b[1]
    caption= b[2]

    button= dlg.Button(x, y, caption)
    x= x+(string.len(caption)+5)
    self.buttons[name]= button
    table.insert(self.everything, button)
  end
end


function SimpleForm:getValue(name)
  local w= self.objects[name]

  if w[1] == "listbox" then
    return w[2]:getCurrent()
  elseif w[1] == "entry" then
    return w[2].entryValue
  elseif w[1] == "check" then
    return w[2].checkboxValue      
  elseif (w[1] == "checklist") or (w[1]=="checktree") then
    local oplist= w[3]
    local tree=w[2]
    local i
    local sel= {}
    for i=1,table.getn(oplist) do
      if tree:getEntryValue(oplist[i][2])[2] == 1 then
        table.insert(sel, oplist[i][1])
      end
    end
    return sel
  else
    return nil
  end
end


function SimpleForm:setHelpLine(help)
  self.helpline= help
end

function SimpleForm:run(dontPopDown)
  local w, _

  dlg.centeredWindow(self.width, self.height, self.title)

  self.form= dlg.Form()

  for _, w in self.everything do
    self.form:add(w)
  end

  result= self.form:run()

  if not(dontPopDown) then
    dlg.popWindow()
  end
  for name, button in self.buttons do
    if button:matchId(result[2])==1 then
      return name
    end
  end
  return nil
end


function SimpleForm:pop()
  dlg.popWindow()
end
