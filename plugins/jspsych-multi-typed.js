/**
 * jspsych-multi-typed
 * Simon Farrell, modified from Josh de Leeuw multi-stim-multi-response
 *
 * plugin for collecting a set of responses
 * in a textbox
 *
 *
 **/


 jsPsych.plugins["multi-typed"] = (function() {

  var plugin = {};

  plugin.trial = function(display_element, trial) {

    // optional parameters
    trial.n_responses = (typeof trial.n_responses === 'undefined') ? 1 : trial.n_responses;
    trial.prompt = (typeof trial.prompt === 'undefined') ? "" : trial.prompt;
    trial.time_limit = trial.time_limit || 30000;

    trial.terminate_key = (typeof trial.terminate_key === 'undefined') ? 0 : trial.terminate_key;
    trial.nMessage = (typeof trial.nMessage === 'undefined') ? true : trial.nMessage;

    // if any trial variables are functions
    // this evaluates the function and replaces
    // it with the output of the function
    trial = jsPsych.pluginAPI.evaluateFunctionParameters(trial);

    // this array holds handlers from setTimeout calls
    // that need to be cleared if the trial ends early
    var setTimeoutHandlers = [];

    // array for response times for each of the different response types
    var responses = [];
    var responseTimes = [];

    console.log(trial.n_responses)

    // function to end trial when it is time
    var end_trial = function(msg) {


      // kill any remaining setTimeout handlers
      for (var i = 0; i < setTimeoutHandlers.length; i++) {
        clearTimeout(setTimeoutHandlers[i]);
      }

      display_element.html(msg);
      display_element.append("<br><br>Press space bar to continue.");

      // pads out missing responses
      while (trial.n_responses>responses.length){
        responses.push("");
        responseTimes.push(-1);
      }

      // gather the data to store for the trial
      var trial_data = {
        "rt": JSON.stringify(responseTimes),
        "responses": JSON.stringify(responses)
      };

      $(document).keydown(function(e){
        if (e.which == 32){
          $(document).unbind('keydown');
          display_element.html('');
          jsPsych.finishTrial(trial_data);
        }
      });

    };

    //show prompt if there is one
    if (trial.prompt !== "") {
      display_element.append(trial.prompt);
    }

    // list recall; typed responses
    var ndone = 0;
    var oldSecs = (new Date()).getTime();

    display_element.append('<br><input name="typed_resp" type="text" id="response" style="font-size: 24pt" autocorrect="on" autocomplete="off" spellcheck="true"/>');

    $("#response").focus();

    $("#response").keydown( function(e){

      if (e.which == 13)
      {
        nowSecs = (new Date()).getTime();
        responses.push($("#response").val());
        responseTimes.push(nowSecs-oldSecs);
        oldSecs = nowSecs;
        $("#response").val("");
        ndone++;
        
        if (ndone>=trial.n_responses){
          if (trial.nMessage){
            end_trial("Thanks, that's enough responses.");
          } else {
            end_trial("");
          }
        }
      } else if (e.which == trial.terminate_key){
        end_trial("Trial terminated.");
      }
    });

    var trialTimer = setTimeout(function () {end_trial("Time is up.");}, trial.time_limit);
    setTimeoutHandlers.push(trialTimer);

  };

  return plugin;
})();
