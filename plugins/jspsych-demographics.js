/** (June 2014, Simon Farrell)
This collects demographic information from the participant. The form is heavily borrowed from 
https://github.com/drewhendrickson/gae-experiment-base (retrieved May 2014)
This is made to be run only as a single trial **/


jsPsych.plugins["demographics"] = (function() {

    var plugin = {};

    plugin.trial = function(display_element, trial) {
        
        var trial = new Array(1);
        trial = {};
        trial.type = "demographics";
        trial.fields = (typeof trial.fields === 'undefined') ? "ID" : trial.fields; // this isn't actually used at present

        // trial[0] = {};
        // trial[0].type = "demographics";
        // trial[0].fields = (typeof trial.fields === 'undefined') ? "ID" : trial.fields; // this isn't actually used at present

        // if any trial variables are functions
        // this evaluates the function and replaces
        // it with the output of the function
        trial = jsPsych.pluginAPI.evaluateFunctionParameters(trial);

        display_element.html('<br> <br> <form name="demogform" id="thedemogform">  \
            <label for="user">UWA ID:*</label><br>\
            <input name="uniqueID" /><br /><br /> \
            <label for="age">Age:</label><input name="age" /><br /><br /> \
            <label for="gender">Gender:</label><input type="radio" name="gender" value="male" /> Male &nbsp; <input type="radio" name="gender" value="female" /> Female &nbsp;<input type="radio" name="gender" value="decline" /> Decline to state<br /><br /> \
            <label for="country">Country:</label> \
            <select name="country" id="country" class="drop-menu"> \
            <option>Afghanistan</option><option>&Aring;land Islands</option><option>Albania</option><option>Algeria</option><option>American Samoa</option><option>Andorra</option><option>Angola</option><option>Anguilla</option><option>Antarctica</option><option>Antigua and Barbuda</option><option>Argentina</option><option>Armenia</option><option>Aruba</option><option  selected="selected">Australia</option><option>Austria</option><option>Azerbaijan</option><option>Bahamas</option><option>Bahrain</option><option>Bangladesh</option><option>Barbados</option><option>Belarus</option><option>Belgium</option><option>Belize</option><option>Benin</option><option>Bermuda</option><option>Bhutan</option><option>Bolivia</option><option>Bosnia and Herzegovina</option><option>Botswana</option><option>Bouvet Island</option><option>Brazil</option><option>British Indian Ocean territory</option><option>Brunei Darussalam</option><option>Bulgaria</option><option>Burkina Faso</option><option>Burundi</option><option>Cambodia</option><option>Cameroon</option><option>Canada</option><option>Cape Verde</option><option>Cayman Islands</option><option>Central African Republic</option><option>Chad</option><option>Chile</option><option>China</option><option>Christmas Island</option><option>Cocos (Keeling) Islands</option><option>Colombia</option><option>Comoros</option><option>Congo</option><option>Congo, Democratic Republic</option><option>Cook Islands</option><option>Costa Rica</option><option>C&ocirc;te dIvoire (Ivory Coast)</option><option>Croatia (Hrvatska)</option><option>Cuba</option><option>Cyprus</option><option>Czech Republic</option><option>Denmark</option><option>Djibouti</option><option>Dominica</option><option>Dominican Republic</option><option>East Timor</option><option>Ecuador</option><option>Egypt</option><option>El Salvador</option><option>Equatorial Guinea</option><option>Eritrea</option><option>Estonia</option><option>Ethiopia</option><option>Falkland Islands</option><option>Faroe Islands</option><option>Fiji</option><option>Finland</option><option >France</option><option>French Guiana</option><option>French Polynesia</option><option>French Southern Territories</option><option>Gabon</option><option>Gambia</option><option>Georgia</option><option >Germany</option><option>Ghana</option><option>Gibraltar</option><option>Greece</option><option>Greenland</option><option>Grenada</option><option>Guadeloupe</option><option>Guam</option><option>Guatemala</option><option>Guinea</option><option>Guinea-Bissau</option><option>Guyana</option><option>Haiti</option><option>Heard and McDonald Islands</option><option>Honduras</option><option>Hong Kong</option><option>Hungary</option><option>Iceland</option><option>India</option><option>Indonesia</option><option>Iran</option><option>Iraq</option><option>Ireland</option><option>Israel</option><option>Italy</option><option>Jamaica</option><option>Japan</option><option>Jordan</option><option>Kazakhstan</option><option>Kenya</option><option>Kiribati</option><option>Korea (north)</option><option>Korea (south)</option><option>Kuwait</option><option>Kyrgyzstan</option><option>Lao Peoples Democratic Republic</option><option>Latvia</option><option>Lebanon</option><option>Lesotho</option><option>Liberia</option><option>Libyan Arab Jamahiriya</option><option>Liechtenstein</option><option>Lithuania</option><option>Luxembourg</option><option>Macao</option><option>Macedonia, Former Yugoslav Republic Of</option><option>Madagascar</option><option>Malawi</option><option>Malaysia</option><option>Maldives</option><option>Mali</option><option>Malta</option><option>Marshall Islands</option><option>Martinique</option><option>Mauritania</option><option>Mauritius</option><option>Mayotte</option><option>Mexico</option><option>Micronesia</option><option>Moldova</option><option>Monaco</option><option>Mongolia</option><option>Montenegro</option><option>Montserrat</option><option>Morocco</option><option>Mozambique</option><option>Myanmar</option><option>Namibia</option><option>Nauru</option><option>Nepal</option><option>Netherlands</option><option>Netherlands Antilles</option><option>New Caledonia</option><option >New Zealand</option><option>Nicaragua</option><option>Niger</option><option>Nigeria</option><option>Niue</option><option>Norfolk Island</option><option>Northern Mariana Islands</option><option>Norway</option><option>Oman</option><option>Pakistan</option><option>Palau</option><option>Palestinian Territories</option><option>Panama</option><option>Papua New Guinea</option><option>Paraguay</option><option>Peru</option><option>Philippines</option><option>Pitcairn</option><option>Poland</option><option>Portugal</option><option>Puerto Rico</option><option>Qatar</option><option>R&eacute;union</option><option>Romania</option><option>Russian Federation</option><option>Rwanda</option><option>Saint Helena</option><option>Saint Kitts and Nevis</option><option>Saint Lucia</option><option>Saint Pierre and Miquelon</option><option>Saint Vincent and the Grenadines</option><option>Samoa</option><option>San Marino</option><option>Sao Tome and Principe</option><option>Saudi Arabia</option><option>Senegal</option><option>Serbia</option><option>Seychelles</option><option>Sierra Leone</option><option>Singapore</option><option>Slovakia</option><option>Slovenia</option><option>Solomon Islands</option><option>Somalia</option><option>South Africa</option><option>South Georgia and the South Sandwich Islands</option><option>Spain</option><option>Sri Lanka</option><option>Sudan</option><option>Suriname</option><option>Svalbard and Jan Mayen Islands</option><option>Swaziland</option><option>Sweden</option><option>Switzerland</option><option>Syria</option><option>Taiwan</option><option>Tajikistan</option><option>Tanzania</option><option>Thailand</option><option>Togo</option><option>Tokelau</option><option>Tonga</option><option>Trinidad and Tobago</option><option>Tunisia</option><option>Turkey</option><option>Turkmenistan</option><option>Turks and Caicos Islands</option><option>Tuvalu</option><option>Uganda</option><option>Ukraine</option><option>United Arab Emirates</option><option >United Kingdom</option><option >United States of America</option><option>Uruguay</option><option>Uzbekistan</option><option>Vanuatu</option><option>Vatican City</option><option>Venezuela</option><option>Vietnam</option><option>Virgin Islands (British)</option><option>Virgin Islands (US)</option><option>Wallis and Futuna Islands</option><option>Western Sahara</option><option>Yemen</option><option>Zaire</option><option>Zambia</option><option>Zimbabwe</option></select> <br><br> \
            <input type="button" name="submit" id="cont-button" value="Continue";"/> \
            </form>\
            <p id="val-message"></p>\
            <p>* We ask for your UWA ID so we can track your participation and assign your credits. Your ID will not be associated with your data.</p>');

        $("#cont-button").click(function() { validateForm() });

        function validateForm() {

            var msg = "";

            var subjID = document.forms["demogform"]["uniqueID"].value;
            if (subjID == null || subjID == "") {
                msg = msg + "<br>Please enter a valid participant ID"
            }
            
            var age = document.forms["demogform"]["age"].value;
            if (age == null || age == "" || isNaN(age) ) {
                msg = msg + "<br>Please enter a valid age";
            } else if (age < 17){
                msg = msg + "<br>You must be at least 18 years of age to participate";
            } else if (age>150){
                msg = msg + "<br>Please enter a valid age";
            }

            var gender = $("input[type='radio'][name='gender']:checked").val();

            if (gender != "male" && gender != "female" && gender != "decline") {
                msg = msg + "<br>Please choose a gender (or select the 'decline' option)";
            }

            var country = document.forms["demogform"]["country"].value;
            if (country == null || country == "") {
                msg = msg + "<br>Please select the country in which you usually reside";
            }
            
            if (msg==""){
                // everything looks good, let's continue
                jsPsych.data.write($.extend({}, {
                    "trial_type": "demographics",
                    "ID": subjID,
                    "age": age,
                    "gender": gender, 
                    "country": country
                }, trial.data));
                display_element.html(''); // clear the display
                jsPsych.finishTrial();
            } else {
                $('#val-message').html("<span style='color:red'>" + msg + "</span>");
            }
        };
    }

    return plugin;
})();
