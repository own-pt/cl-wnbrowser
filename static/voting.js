var pending = false;

window.addEventListener(
  "beforeunload", 
  function (e)
  {
    if (pending)
    {
      var confirmationMessage = 'There are still changes being sent to the server.  If you leave now, your changes will be lost.';
      
      (e || window.event).returnValue = confirmationMessage; //Gecko + IE
      return confirmationMessage; //Gecko + Webkit, Safari, Chrome etc.
    }
  });

var votes = {};

function registerVote(suggestion_id, pvote_id, nvote_id)
{
  var vote_id = null;
  if (pvote_id) vote_id = pvote_id;
  if (nvote_id) vote_id = nvote_id;
  votes[suggestion_id] = vote_id;
}

function getVote(suggestion_id)
{
  return votes[suggestion_id];
}

function update(suggestion_id, pos, neg)
{
  if (suggestion_id)
  {
    var pos_elt = $('#positive-' + suggestion_id);
    
    pos_value = parseInt(pos_elt.text(), 10);
    pos_value += pos;
    
    var neg_elt = $('#negative-' + suggestion_id);
    
    neg_value = parseInt(neg_elt.text(), 10);
    neg_value += neg;
    
    var total_elt = $('#total-' + suggestion_id);
    
    pos_elt.text(pos_value);
    neg_elt.text(neg_value);
    total_elt.text(pos_value + neg_value);
  }
}

function isPressed(btn)
{
  return btn.hasClass('upmod') || btn.hasClass('downmod');
}

function dePress(btn)
{
  if (btn.hasClass('upmod'))
  {
    btn.removeClass('upmod');
    btn.addClass('up');
  } else
  {
    if (btn.hasClass('downmod'))
    {
      btn.removeClass('downmod');
      btn.addClass('down');
    }
  }
}

function press(btn)
{
  if (btn.hasClass('up'))
  {
    btn.removeClass('up');
    btn.addClass('upmod');
  } else
  {
    if (btn.hasClass('down'))
    {
      btn.removeClass('down');
      btn.addClass('downmod');
    }
  }
}

function callVoteUp(suggestion_id, callback)
{
  if (suggestion_id)
  {
    pending = true;
    $.ajax({ 
      url: 'vote-up',
      data: { id: suggestion_id },
      dataType: "json",
      complete:
      function(jqXHR, status)
      {
        pending = false;
      },
      success:
      function(data, status, jqXHR)
      {
        registerVote(suggestion_id, data.id, null);
      }
    });
  }
}

function callVoteDown(suggestion_id, callback)
{
  if (suggestion_id)
  {
    pending = true;
    $.ajax({ 
      url: 'vote-down',
      data: { id: suggestion_id },
      dataType: "json",
      complete:
      function(jqXHR, status)
      {
        pending = false;
      },
      success:
      function(data, status, jqXHR)
      {
        registerVote(suggestion_id, null, data.id);
      }
    });
  }
}

function callDeleteVote(vote_id, callback)
{
  if (vote_id) 
  {
    pending = true;
    $.ajax({ 
      url: 'delete-vote',
      data: { id: vote_id },
      dataType: "json",
      complete:
      function(jqXHR, status)
      {
        pending = false;
      },
      success:
      function(data, status)
      {
        if (callback) callback(data, status); 
      }
    });
  }
}

function voteup(suggestion_id)
{
  if (pending)
    return;
  
  $('#warning').css('display', 'block');
  
  var button_up = $('#vote-up-'+suggestion_id);
  var button_down = $('#vote-down-'+suggestion_id);
  
  var vote_id = getVote(suggestion_id);
  
  // up was already pressed: need to delete the vote
  if (isPressed(button_up))
  {
    dePress(button_up);
    update(suggestion_id, -1, 0);
    callDeleteVote(vote_id);
  }
  else
  {
    // down was already pressed: need to delete that vote and add this new one
    if (isPressed(button_down))
    {
      dePress(button_down);
      update(suggestion_id, 0, 1);
      press(button_up);
      update(suggestion_id, 1, 0);
      
      callDeleteVote(
        vote_id,
        function(data,status)
        {
          callVoteUp(suggestion_id);
        });
    }
    else 
    {
      press(button_up);
      update(suggestion_id, 1, 0);
      callVoteUp(suggestion_id);
    }
  }
}

function votedown(suggestion_id)
{
  if (pending)
    return;
  
  $('#warning').css('display', 'block');
  
  var button_up = $('#vote-up-'+suggestion_id);
  var button_down = $('#vote-down-'+suggestion_id);
  
  var vote_id = getVote(suggestion_id);
  
  if (isPressed(button_down))
  {
    dePress(button_down);
    update(suggestion_id, 0, 1);
    callDeleteVote(vote_id);
  }
  else
  {
    if (isPressed(button_up))
    {
      dePress(button_up);
      update(suggestion_id, -1, 0);
      press(button_down);
      update(suggestion_id, 0, -1);
      callDeleteVote(
        vote_id,
        function(data,status)
        {
          callVoteDown(suggestion_id);
        });
    }
    else
    {
      press(button_down);
      update(suggestion_id, 0, -1);
      callVoteDown(suggestion_id);
    }
  }
}

function toggle_visibility(id) 
{
  var e = document.getElementById(id);
  
  if(e.style.display == 'block')
    e.style.display = 'none';
  else
    e.style.display = 'block';
}
