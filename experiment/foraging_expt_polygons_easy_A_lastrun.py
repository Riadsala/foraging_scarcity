#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
This experiment was created using PsychoPy3 Experiment Builder (v2021.2.3),
    on April 18, 2023, at 13:26
If you publish work using this script the most relevant publication is:

    Peirce J, Gray JR, Simpson S, MacAskill M, Höchenberger R, Sogo H, Kastman E, Lindeløv JK. (2019) 
        PsychoPy2: Experiments in behavior made easy Behav Res 51: 195. 
        https://doi.org/10.3758/s13428-018-01193-y

"""

from __future__ import absolute_import, division

from psychopy import locale_setup
from psychopy import prefs
from psychopy import sound, gui, visual, core, data, event, logging, clock, colors
from psychopy.constants import (NOT_STARTED, STARTED, PLAYING, PAUSED,
                                STOPPED, FINISHED, PRESSED, RELEASED, FOREVER)

import numpy as np  # whole numpy lib is available, prepend 'np.'
from numpy import (sin, cos, tan, log, log10, pi, average,
                   sqrt, std, deg2rad, rad2deg, linspace, asarray)
from numpy.random import random, randint, normal, shuffle, choice as randchoice
import os  # handy system and path functions
import sys  # to get file system encoding

from psychopy.hardware import keyboard



# Ensure that relative paths start from the same directory as this script
_thisDir = os.path.dirname(os.path.abspath(__file__))
os.chdir(_thisDir)

# Store info about the experiment session
psychopyVersion = '2021.2.3'
expName = 'foraging_expt_polygons_easy_A'  # from the Builder filename that created this script
expInfo = {'participant': '', 'session': '001'}
dlg = gui.DlgFromDict(dictionary=expInfo, sortKeys=False, title=expName)
if dlg.OK == False:
    core.quit()  # user pressed cancel
expInfo['date'] = data.getDateStr()  # add a simple timestamp
expInfo['expName'] = expName
expInfo['psychopyVersion'] = psychopyVersion

# Data file name stem = absolute path + name; later add .psyexp, .csv, .log, etc
filename = _thisDir + os.sep + u'data/%s_%s_%s' % (expInfo['participant'], expName, expInfo['date'])

# An ExperimentHandler isn't essential but helps with data saving
thisExp = data.ExperimentHandler(name=expName, version='',
    extraInfo=expInfo, runtimeInfo=None,
    originPath='D:\\Dropbox\\CurrentResearch\\Foraging_simple\\foraging_rr\\feature_conjunction_version\\foraging_expt_polygons_easy_A_lastrun.py',
    savePickle=True, saveWideText=True,
    dataFileName=filename)
# save a log file for detail verbose info
logFile = logging.LogFile(filename+'.log', level=logging.EXP)
logging.console.setLevel(logging.WARNING)  # this outputs to the screen, not a file

endExpNow = False  # flag for 'escape' or other condition => quit the exp
frameTolerance = 0.001  # how close to onset before 'same' frame

# Start Code - component code to be run after the window creation

# Setup the Window
win = visual.Window(
    size=[960, 540], fullscr=False, screen=0, 
    winType='pyglet', allowGUI=True, allowStencil=False,
    monitor='testMonitor', color=[0,0,0], colorSpace='rgb',
    blendMode='avg', useFBO=True, 
    units='height')
# store frame rate of monitor if we can measure it
expInfo['frameRate'] = win.getActualFrameRate()
if expInfo['frameRate'] != None:
    frameDur = 1.0 / round(expInfo['frameRate'])
else:
    frameDur = 1.0 / 60.0  # could not measure, so guess

# Setup eyetracking
ioDevice = ioConfig = ioSession = ioServer = eyetracker = None

# create a default keyboard (e.g. to check for escape)
defaultKeyboard = keyboard.Keyboard()

# Initialize components for Routine "instructions"
instructionsClock = core.Clock()
text = visual.TextStim(win=win, name='text',
    text='Click on all the red and green targets.\n\nClick to continue.',
    font='Open Sans',
    pos=(0, 0), height=0.05, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=0.0);
mouse_2 = event.Mouse(win=win)
x, y = [None, None]
mouse_2.mouseClock = core.Clock()

# Initialize components for Routine "instructions_2"
instructions_2Clock = core.Clock()
instructions_text_2 = visual.TextStim(win=win, name='instructions_text_2',
    text='Ignore any other colours!\n\nIf you click on these, you will need to repeat the trial.\n\nYou need to complete 5 trials, and will have 20 seconds on each trial.',
    font='Open Sans',
    pos=(0, 0), height=0.05, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=0.0);
mouse_3 = event.Mouse(win=win)
x, y = [None, None]
mouse_3.mouseClock = core.Clock()

# Initialize components for Routine "trial"
trialClock = core.Clock()
polygon = visual.ShapeStim(
    win=win, name='polygon', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='red', fillColor='red',
    opacity=None, depth=0.0, interpolate=True)
polygon_2 = visual.ShapeStim(
    win=win, name='polygon_2', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='red', fillColor='red',
    opacity=None, depth=-1.0, interpolate=True)
polygon_3 = visual.ShapeStim(
    win=win, name='polygon_3', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='red', fillColor='red',
    opacity=None, depth=-2.0, interpolate=True)
polygon_4 = visual.ShapeStim(
    win=win, name='polygon_4', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='red', fillColor='red',
    opacity=None, depth=-3.0, interpolate=True)
polygon_5 = visual.ShapeStim(
    win=win, name='polygon_5', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='red', fillColor='red',
    opacity=None, depth=-4.0, interpolate=True)
polygon_6 = visual.ShapeStim(
    win=win, name='polygon_6', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='red', fillColor='red',
    opacity=None, depth=-5.0, interpolate=True)
polygon_7 = visual.ShapeStim(
    win=win, name='polygon_7', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='red', fillColor='red',
    opacity=None, depth=-6.0, interpolate=True)
polygon_8 = visual.ShapeStim(
    win=win, name='polygon_8', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='red', fillColor='red',
    opacity=None, depth=-7.0, interpolate=True)
polygon_9 = visual.ShapeStim(
    win=win, name='polygon_9', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='red', fillColor='red',
    opacity=None, depth=-8.0, interpolate=True)
polygon_10 = visual.ShapeStim(
    win=win, name='polygon_10', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='red', fillColor='red',
    opacity=None, depth=-9.0, interpolate=True)
polygon_11 = visual.ShapeStim(
    win=win, name='polygon_11', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='red', fillColor='red',
    opacity=None, depth=-10.0, interpolate=True)
polygon_12 = visual.ShapeStim(
    win=win, name='polygon_12', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='red', fillColor='red',
    opacity=None, depth=-11.0, interpolate=True)
polygon_13 = visual.ShapeStim(
    win=win, name='polygon_13', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='red', fillColor='red',
    opacity=None, depth=-12.0, interpolate=True)
polygon_14 = visual.ShapeStim(
    win=win, name='polygon_14', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='red', fillColor='red',
    opacity=None, depth=-13.0, interpolate=True)
polygon_15 = visual.ShapeStim(
    win=win, name='polygon_15', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='red', fillColor='red',
    opacity=None, depth=-14.0, interpolate=True)
polygon_16 = visual.ShapeStim(
    win=win, name='polygon_16', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='lime', fillColor='lime',
    opacity=None, depth=-15.0, interpolate=True)
polygon_17 = visual.ShapeStim(
    win=win, name='polygon_17', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='lime', fillColor='lime',
    opacity=None, depth=-16.0, interpolate=True)
polygon_18 = visual.ShapeStim(
    win=win, name='polygon_18', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='lime', fillColor='lime',
    opacity=None, depth=-17.0, interpolate=True)
polygon_19 = visual.ShapeStim(
    win=win, name='polygon_19', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='lime', fillColor='lime',
    opacity=None, depth=-18.0, interpolate=True)
polygon_20 = visual.ShapeStim(
    win=win, name='polygon_20', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='lime', fillColor='lime',
    opacity=None, depth=-19.0, interpolate=True)
polygon_21 = visual.ShapeStim(
    win=win, name='polygon_21', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='blue', fillColor='blue',
    opacity=None, depth=-20.0, interpolate=True)
polygon_22 = visual.ShapeStim(
    win=win, name='polygon_22', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='blue', fillColor='blue',
    opacity=None, depth=-21.0, interpolate=True)
polygon_23 = visual.ShapeStim(
    win=win, name='polygon_23', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='blue', fillColor='blue',
    opacity=None, depth=-22.0, interpolate=True)
polygon_24 = visual.ShapeStim(
    win=win, name='polygon_24', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='blue', fillColor='blue',
    opacity=None, depth=-23.0, interpolate=True)
polygon_25 = visual.ShapeStim(
    win=win, name='polygon_25', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='blue', fillColor='blue',
    opacity=None, depth=-24.0, interpolate=True)
polygon_26 = visual.ShapeStim(
    win=win, name='polygon_26', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='blue', fillColor='blue',
    opacity=None, depth=-25.0, interpolate=True)
polygon_27 = visual.ShapeStim(
    win=win, name='polygon_27', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='blue', fillColor='blue',
    opacity=None, depth=-26.0, interpolate=True)
polygon_28 = visual.ShapeStim(
    win=win, name='polygon_28', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='blue', fillColor='blue',
    opacity=None, depth=-27.0, interpolate=True)
polygon_29 = visual.ShapeStim(
    win=win, name='polygon_29', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='blue', fillColor='blue',
    opacity=None, depth=-28.0, interpolate=True)
polygon_30 = visual.ShapeStim(
    win=win, name='polygon_30', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='blue', fillColor='blue',
    opacity=None, depth=-29.0, interpolate=True)
polygon_31 = visual.ShapeStim(
    win=win, name='polygon_31', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='yellow', fillColor='yellow',
    opacity=None, depth=-30.0, interpolate=True)
polygon_32 = visual.ShapeStim(
    win=win, name='polygon_32', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='yellow', fillColor='yellow',
    opacity=None, depth=-31.0, interpolate=True)
polygon_33 = visual.ShapeStim(
    win=win, name='polygon_33', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='yellow', fillColor='yellow',
    opacity=None, depth=-32.0, interpolate=True)
polygon_34 = visual.ShapeStim(
    win=win, name='polygon_34', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='yellow', fillColor='yellow',
    opacity=None, depth=-33.0, interpolate=True)
polygon_35 = visual.ShapeStim(
    win=win, name='polygon_35', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='yellow', fillColor='yellow',
    opacity=None, depth=-34.0, interpolate=True)
polygon_36 = visual.ShapeStim(
    win=win, name='polygon_36', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='yellow', fillColor='yellow',
    opacity=None, depth=-35.0, interpolate=True)
polygon_37 = visual.ShapeStim(
    win=win, name='polygon_37', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='yellow', fillColor='yellow',
    opacity=None, depth=-36.0, interpolate=True)
polygon_38 = visual.ShapeStim(
    win=win, name='polygon_38', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='yellow', fillColor='yellow',
    opacity=None, depth=-37.0, interpolate=True)
polygon_39 = visual.ShapeStim(
    win=win, name='polygon_39', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='yellow', fillColor='yellow',
    opacity=None, depth=-38.0, interpolate=True)
polygon_40 = visual.ShapeStim(
    win=win, name='polygon_40', vertices=99,
    size=(0.04, 0.04),
    ori=0.0, pos=[0,0],
    lineWidth=1.0,     colorSpace='rgb',  lineColor='yellow', fillColor='yellow',
    opacity=None, depth=-39.0, interpolate=True)
points = 0
myCount = 0
distractorClick = 0
mouse = event.Mouse(win=win)
x, y = [None, None]
mouse.mouseClock = core.Clock()

# Initialize components for Routine "feedback"
feedbackClock = core.Clock()
feedback_text_2 = visual.TextStim(win=win, name='feedback_text_2',
    text='',
    font='Open Sans',
    pos=(0, 0.1), height=0.1, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=0.0);
feedback_text = visual.TextStim(win=win, name='feedback_text',
    text='',
    font='Open Sans',
    pos=(0, -0.1), height=0.1, wrapWidth=None, ori=0.0, 
    color='white', colorSpace='rgb', opacity=None, 
    languageStyle='LTR',
    depth=-1.0);

# Create some handy timers
globalClock = core.Clock()  # to track the time since experiment started
routineTimer = core.CountdownTimer()  # to track time remaining of each (non-slip) routine 

# ------Prepare to start Routine "instructions"-------
continueRoutine = True
# update component parameters for each repeat
# setup some python lists for storing info about the mouse_2
mouse_2.x = []
mouse_2.y = []
mouse_2.leftButton = []
mouse_2.midButton = []
mouse_2.rightButton = []
mouse_2.time = []
gotValidClick = False  # until a click is received
# keep track of which components have finished
instructionsComponents = [text, mouse_2]
for thisComponent in instructionsComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
instructionsClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "instructions"-------
while continueRoutine:
    # get current time
    t = instructionsClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=instructionsClock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *text* updates
    if text.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        text.frameNStart = frameN  # exact frame index
        text.tStart = t  # local t and not account for scr refresh
        text.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(text, 'tStartRefresh')  # time at next scr refresh
        text.setAutoDraw(True)
    # *mouse_2* updates
    if mouse_2.status == NOT_STARTED and t >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        mouse_2.frameNStart = frameN  # exact frame index
        mouse_2.tStart = t  # local t and not account for scr refresh
        mouse_2.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(mouse_2, 'tStartRefresh')  # time at next scr refresh
        mouse_2.status = STARTED
        mouse_2.mouseClock.reset()
        prevButtonState = mouse_2.getPressed()  # if button is down already this ISN'T a new click
    if mouse_2.status == STARTED:  # only update if started and not finished!
        buttons = mouse_2.getPressed()
        if buttons != prevButtonState:  # button state changed?
            prevButtonState = buttons
            if sum(buttons) > 0:  # state changed to a new click
                x, y = mouse_2.getPos()
                mouse_2.x.append(x)
                mouse_2.y.append(y)
                buttons = mouse_2.getPressed()
                mouse_2.leftButton.append(buttons[0])
                mouse_2.midButton.append(buttons[1])
                mouse_2.rightButton.append(buttons[2])
                mouse_2.time.append(mouse_2.mouseClock.getTime())
                # abort routine on response
                continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in instructionsComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "instructions"-------
for thisComponent in instructionsComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
thisExp.addData('text.started', text.tStartRefresh)
thisExp.addData('text.stopped', text.tStopRefresh)
# store data for thisExp (ExperimentHandler)
if len(mouse_2.x): thisExp.addData('mouse_2.x', mouse_2.x[0])
if len(mouse_2.y): thisExp.addData('mouse_2.y', mouse_2.y[0])
if len(mouse_2.leftButton): thisExp.addData('mouse_2.leftButton', mouse_2.leftButton[0])
if len(mouse_2.midButton): thisExp.addData('mouse_2.midButton', mouse_2.midButton[0])
if len(mouse_2.rightButton): thisExp.addData('mouse_2.rightButton', mouse_2.rightButton[0])
if len(mouse_2.time): thisExp.addData('mouse_2.time', mouse_2.time[0])
thisExp.addData('mouse_2.started', mouse_2.tStart)
thisExp.addData('mouse_2.stopped', mouse_2.tStop)
thisExp.nextEntry()
# the Routine "instructions" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# ------Prepare to start Routine "instructions_2"-------
continueRoutine = True
# update component parameters for each repeat
# setup some python lists for storing info about the mouse_3
gotValidClick = False  # until a click is received
# keep track of which components have finished
instructions_2Components = [instructions_text_2, mouse_3]
for thisComponent in instructions_2Components:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
instructions_2Clock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "instructions_2"-------
while continueRoutine:
    # get current time
    t = instructions_2Clock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=instructions_2Clock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *instructions_text_2* updates
    if instructions_text_2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        instructions_text_2.frameNStart = frameN  # exact frame index
        instructions_text_2.tStart = t  # local t and not account for scr refresh
        instructions_text_2.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(instructions_text_2, 'tStartRefresh')  # time at next scr refresh
        instructions_text_2.setAutoDraw(True)
    # *mouse_3* updates
    if mouse_3.status == NOT_STARTED and t >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        mouse_3.frameNStart = frameN  # exact frame index
        mouse_3.tStart = t  # local t and not account for scr refresh
        mouse_3.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(mouse_3, 'tStartRefresh')  # time at next scr refresh
        mouse_3.status = STARTED
        mouse_3.mouseClock.reset()
        prevButtonState = mouse_3.getPressed()  # if button is down already this ISN'T a new click
    if mouse_3.status == STARTED:  # only update if started and not finished!
        buttons = mouse_3.getPressed()
        if buttons != prevButtonState:  # button state changed?
            prevButtonState = buttons
            if sum(buttons) > 0:  # state changed to a new click
                # abort routine on response
                continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in instructions_2Components:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "instructions_2"-------
for thisComponent in instructions_2Components:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
thisExp.addData('instructions_text_2.started', instructions_text_2.tStartRefresh)
thisExp.addData('instructions_text_2.stopped', instructions_text_2.tStopRefresh)
# store data for thisExp (ExperimentHandler)
x, y = mouse_3.getPos()
buttons = mouse_3.getPressed()
thisExp.addData('mouse_3.x', x)
thisExp.addData('mouse_3.y', y)
thisExp.addData('mouse_3.leftButton', buttons[0])
thisExp.addData('mouse_3.midButton', buttons[1])
thisExp.addData('mouse_3.rightButton', buttons[2])
thisExp.addData('mouse_3.started', mouse_3.tStart)
thisExp.addData('mouse_3.stopped', mouse_3.tStop)
thisExp.nextEntry()
# the Routine "instructions_2" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
trials = data.TrialHandler(nReps=1.0, method='random', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('final_target_positions.xlsx'),
    seed=None, name='trials')
thisExp.addLoop(trials)  # add the loop to the experiment
thisTrial = trials.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisTrial.rgb)
if thisTrial != None:
    for paramName in thisTrial:
        exec('{} = thisTrial[paramName]'.format(paramName))

for thisTrial in trials:
    currentLoop = trials
    # abbreviate parameter names if possible (e.g. rgb = thisTrial.rgb)
    if thisTrial != None:
        for paramName in thisTrial:
            exec('{} = thisTrial[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "trial"-------
    continueRoutine = True
    routineTimer.add(20.000000)
    # update component parameters for each repeat
    polygon.setPos([p1_x, p1_y])
    polygon_2.setPos([p2_x, p2_y])
    polygon_3.setPos([p3_x, p3_y])
    polygon_4.setPos([p4_x, p4_y])
    polygon_5.setPos([p5_x, p5_y])
    polygon_6.setPos([p6_x, p6_y])
    polygon_7.setPos([p7_x, p7_y])
    polygon_8.setPos([p8_x, p8_y])
    polygon_9.setPos([p9_x, p9_y])
    polygon_10.setPos([p10_x, p10_y])
    polygon_11.setPos([p11_x, p11_y])
    polygon_12.setPos([p12_x, p12_y])
    polygon_13.setPos([p13_x, p13_y])
    polygon_14.setPos([p14_x, p14_y])
    polygon_15.setPos([p15_x, p15_y])
    polygon_16.setPos([p16_x, p16_y])
    polygon_17.setPos([p17_x, p17_y])
    polygon_18.setPos([p18_x, p18_y])
    polygon_19.setPos([p19_x, p19_y])
    polygon_20.setPos([p20_x, p20_y])
    polygon_21.setPos([p21_x, p21_y])
    polygon_22.setPos([p22_x, p22_y])
    polygon_23.setPos([p23_x, p23_y])
    polygon_24.setPos([p24_x, p24_y])
    polygon_25.setPos([p25_x, p25_y])
    polygon_26.setPos([p26_x, p26_y])
    polygon_27.setPos([p27_x, p27_y])
    polygon_28.setPos([p28_x, p28_y])
    polygon_29.setPos([p29_x, p29_y])
    polygon_30.setPos([p30_x, p30_y])
    polygon_31.setPos([p31_x, p31_y])
    polygon_32.setPos([p32_x, p32_y])
    polygon_33.setPos([p33_x, p33_y])
    polygon_34.setPos([p34_x, p34_y])
    polygon_35.setPos([p35_x, p35_y])
    polygon_36.setPos([p36_x, p36_y])
    polygon_37.setPos([p37_x, p37_y])
    polygon_38.setPos([p38_x, p38_y])
    polygon_39.setPos([p39_x, p39_y])
    polygon_40.setPos([p40_x, p40_y])
    stimuli = [polygon, polygon_2, polygon_3, polygon_4, polygon_5, polygon_6, polygon_7, polygon_8, polygon_9, polygon_10,
    polygon_11, polygon_12, polygon_13, polygon_14, polygon_15, polygon_16, polygon_17, polygon_18, polygon_19, polygon_20,
    polygon_21, polygon_22, polygon_23, polygon_24, polygon_25, polygon_26, polygon_27, polygon_28, polygon_29, polygon_30,
    polygon_31, polygon_32, polygon_33, polygon_34, polygon_35, polygon_36, polygon_37, polygon_38, polygon_39, polygon_40]
    
    for clicked_shape in stimuli:
        thisOri = randint(0,360)
        clicked_shape.setOri(thisOri)
        x_jitter = np.random.uniform(-0.02, 0.02)
        y_jitter = np.random.uniform(-0.05, 0.05)
        clicked_shape.pos += (x_jitter, y_jitter)
        
    bufferTime=.3
    clickClock=core.Clock()
    lastClickTime = 0
    trial_points = 0
    # setup some python lists for storing info about the mouse
    mouse.x = []
    mouse.y = []
    mouse.leftButton = []
    mouse.midButton = []
    mouse.rightButton = []
    mouse.time = []
    mouse.clicked_name = []
    gotValidClick = False  # until a click is received
    # keep track of which components have finished
    trialComponents = [polygon, polygon_2, polygon_3, polygon_4, polygon_5, polygon_6, polygon_7, polygon_8, polygon_9, polygon_10, polygon_11, polygon_12, polygon_13, polygon_14, polygon_15, polygon_16, polygon_17, polygon_18, polygon_19, polygon_20, polygon_21, polygon_22, polygon_23, polygon_24, polygon_25, polygon_26, polygon_27, polygon_28, polygon_29, polygon_30, polygon_31, polygon_32, polygon_33, polygon_34, polygon_35, polygon_36, polygon_37, polygon_38, polygon_39, polygon_40, mouse]
    for thisComponent in trialComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    trialClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "trial"-------
    while continueRoutine and routineTimer.getTime() > 0:
        # get current time
        t = trialClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=trialClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *polygon* updates
        if polygon.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon.frameNStart = frameN  # exact frame index
            polygon.tStart = t  # local t and not account for scr refresh
            polygon.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon, 'tStartRefresh')  # time at next scr refresh
            polygon.setAutoDraw(True)
        if polygon.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon.tStop = t  # not accounting for scr refresh
                polygon.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon, 'tStopRefresh')  # time at next scr refresh
                polygon.setAutoDraw(False)
        
        # *polygon_2* updates
        if polygon_2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_2.frameNStart = frameN  # exact frame index
            polygon_2.tStart = t  # local t and not account for scr refresh
            polygon_2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_2, 'tStartRefresh')  # time at next scr refresh
            polygon_2.setAutoDraw(True)
        if polygon_2.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_2.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_2.tStop = t  # not accounting for scr refresh
                polygon_2.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_2, 'tStopRefresh')  # time at next scr refresh
                polygon_2.setAutoDraw(False)
        
        # *polygon_3* updates
        if polygon_3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_3.frameNStart = frameN  # exact frame index
            polygon_3.tStart = t  # local t and not account for scr refresh
            polygon_3.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_3, 'tStartRefresh')  # time at next scr refresh
            polygon_3.setAutoDraw(True)
        if polygon_3.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_3.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_3.tStop = t  # not accounting for scr refresh
                polygon_3.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_3, 'tStopRefresh')  # time at next scr refresh
                polygon_3.setAutoDraw(False)
        
        # *polygon_4* updates
        if polygon_4.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_4.frameNStart = frameN  # exact frame index
            polygon_4.tStart = t  # local t and not account for scr refresh
            polygon_4.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_4, 'tStartRefresh')  # time at next scr refresh
            polygon_4.setAutoDraw(True)
        if polygon_4.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_4.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_4.tStop = t  # not accounting for scr refresh
                polygon_4.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_4, 'tStopRefresh')  # time at next scr refresh
                polygon_4.setAutoDraw(False)
        
        # *polygon_5* updates
        if polygon_5.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_5.frameNStart = frameN  # exact frame index
            polygon_5.tStart = t  # local t and not account for scr refresh
            polygon_5.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_5, 'tStartRefresh')  # time at next scr refresh
            polygon_5.setAutoDraw(True)
        if polygon_5.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_5.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_5.tStop = t  # not accounting for scr refresh
                polygon_5.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_5, 'tStopRefresh')  # time at next scr refresh
                polygon_5.setAutoDraw(False)
        
        # *polygon_6* updates
        if polygon_6.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_6.frameNStart = frameN  # exact frame index
            polygon_6.tStart = t  # local t and not account for scr refresh
            polygon_6.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_6, 'tStartRefresh')  # time at next scr refresh
            polygon_6.setAutoDraw(True)
        if polygon_6.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_6.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_6.tStop = t  # not accounting for scr refresh
                polygon_6.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_6, 'tStopRefresh')  # time at next scr refresh
                polygon_6.setAutoDraw(False)
        
        # *polygon_7* updates
        if polygon_7.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_7.frameNStart = frameN  # exact frame index
            polygon_7.tStart = t  # local t and not account for scr refresh
            polygon_7.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_7, 'tStartRefresh')  # time at next scr refresh
            polygon_7.setAutoDraw(True)
        if polygon_7.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_7.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_7.tStop = t  # not accounting for scr refresh
                polygon_7.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_7, 'tStopRefresh')  # time at next scr refresh
                polygon_7.setAutoDraw(False)
        
        # *polygon_8* updates
        if polygon_8.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_8.frameNStart = frameN  # exact frame index
            polygon_8.tStart = t  # local t and not account for scr refresh
            polygon_8.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_8, 'tStartRefresh')  # time at next scr refresh
            polygon_8.setAutoDraw(True)
        if polygon_8.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_8.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_8.tStop = t  # not accounting for scr refresh
                polygon_8.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_8, 'tStopRefresh')  # time at next scr refresh
                polygon_8.setAutoDraw(False)
        
        # *polygon_9* updates
        if polygon_9.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_9.frameNStart = frameN  # exact frame index
            polygon_9.tStart = t  # local t and not account for scr refresh
            polygon_9.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_9, 'tStartRefresh')  # time at next scr refresh
            polygon_9.setAutoDraw(True)
        if polygon_9.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_9.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_9.tStop = t  # not accounting for scr refresh
                polygon_9.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_9, 'tStopRefresh')  # time at next scr refresh
                polygon_9.setAutoDraw(False)
        
        # *polygon_10* updates
        if polygon_10.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_10.frameNStart = frameN  # exact frame index
            polygon_10.tStart = t  # local t and not account for scr refresh
            polygon_10.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_10, 'tStartRefresh')  # time at next scr refresh
            polygon_10.setAutoDraw(True)
        if polygon_10.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_10.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_10.tStop = t  # not accounting for scr refresh
                polygon_10.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_10, 'tStopRefresh')  # time at next scr refresh
                polygon_10.setAutoDraw(False)
        
        # *polygon_11* updates
        if polygon_11.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_11.frameNStart = frameN  # exact frame index
            polygon_11.tStart = t  # local t and not account for scr refresh
            polygon_11.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_11, 'tStartRefresh')  # time at next scr refresh
            polygon_11.setAutoDraw(True)
        if polygon_11.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_11.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_11.tStop = t  # not accounting for scr refresh
                polygon_11.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_11, 'tStopRefresh')  # time at next scr refresh
                polygon_11.setAutoDraw(False)
        
        # *polygon_12* updates
        if polygon_12.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_12.frameNStart = frameN  # exact frame index
            polygon_12.tStart = t  # local t and not account for scr refresh
            polygon_12.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_12, 'tStartRefresh')  # time at next scr refresh
            polygon_12.setAutoDraw(True)
        if polygon_12.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_12.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_12.tStop = t  # not accounting for scr refresh
                polygon_12.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_12, 'tStopRefresh')  # time at next scr refresh
                polygon_12.setAutoDraw(False)
        
        # *polygon_13* updates
        if polygon_13.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_13.frameNStart = frameN  # exact frame index
            polygon_13.tStart = t  # local t and not account for scr refresh
            polygon_13.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_13, 'tStartRefresh')  # time at next scr refresh
            polygon_13.setAutoDraw(True)
        if polygon_13.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_13.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_13.tStop = t  # not accounting for scr refresh
                polygon_13.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_13, 'tStopRefresh')  # time at next scr refresh
                polygon_13.setAutoDraw(False)
        
        # *polygon_14* updates
        if polygon_14.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_14.frameNStart = frameN  # exact frame index
            polygon_14.tStart = t  # local t and not account for scr refresh
            polygon_14.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_14, 'tStartRefresh')  # time at next scr refresh
            polygon_14.setAutoDraw(True)
        if polygon_14.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_14.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_14.tStop = t  # not accounting for scr refresh
                polygon_14.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_14, 'tStopRefresh')  # time at next scr refresh
                polygon_14.setAutoDraw(False)
        
        # *polygon_15* updates
        if polygon_15.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_15.frameNStart = frameN  # exact frame index
            polygon_15.tStart = t  # local t and not account for scr refresh
            polygon_15.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_15, 'tStartRefresh')  # time at next scr refresh
            polygon_15.setAutoDraw(True)
        if polygon_15.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_15.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_15.tStop = t  # not accounting for scr refresh
                polygon_15.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_15, 'tStopRefresh')  # time at next scr refresh
                polygon_15.setAutoDraw(False)
        
        # *polygon_16* updates
        if polygon_16.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_16.frameNStart = frameN  # exact frame index
            polygon_16.tStart = t  # local t and not account for scr refresh
            polygon_16.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_16, 'tStartRefresh')  # time at next scr refresh
            polygon_16.setAutoDraw(True)
        if polygon_16.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_16.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_16.tStop = t  # not accounting for scr refresh
                polygon_16.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_16, 'tStopRefresh')  # time at next scr refresh
                polygon_16.setAutoDraw(False)
        
        # *polygon_17* updates
        if polygon_17.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_17.frameNStart = frameN  # exact frame index
            polygon_17.tStart = t  # local t and not account for scr refresh
            polygon_17.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_17, 'tStartRefresh')  # time at next scr refresh
            polygon_17.setAutoDraw(True)
        if polygon_17.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_17.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_17.tStop = t  # not accounting for scr refresh
                polygon_17.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_17, 'tStopRefresh')  # time at next scr refresh
                polygon_17.setAutoDraw(False)
        
        # *polygon_18* updates
        if polygon_18.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_18.frameNStart = frameN  # exact frame index
            polygon_18.tStart = t  # local t and not account for scr refresh
            polygon_18.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_18, 'tStartRefresh')  # time at next scr refresh
            polygon_18.setAutoDraw(True)
        if polygon_18.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_18.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_18.tStop = t  # not accounting for scr refresh
                polygon_18.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_18, 'tStopRefresh')  # time at next scr refresh
                polygon_18.setAutoDraw(False)
        
        # *polygon_19* updates
        if polygon_19.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_19.frameNStart = frameN  # exact frame index
            polygon_19.tStart = t  # local t and not account for scr refresh
            polygon_19.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_19, 'tStartRefresh')  # time at next scr refresh
            polygon_19.setAutoDraw(True)
        if polygon_19.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_19.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_19.tStop = t  # not accounting for scr refresh
                polygon_19.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_19, 'tStopRefresh')  # time at next scr refresh
                polygon_19.setAutoDraw(False)
        
        # *polygon_20* updates
        if polygon_20.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_20.frameNStart = frameN  # exact frame index
            polygon_20.tStart = t  # local t and not account for scr refresh
            polygon_20.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_20, 'tStartRefresh')  # time at next scr refresh
            polygon_20.setAutoDraw(True)
        if polygon_20.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_20.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_20.tStop = t  # not accounting for scr refresh
                polygon_20.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_20, 'tStopRefresh')  # time at next scr refresh
                polygon_20.setAutoDraw(False)
        
        # *polygon_21* updates
        if polygon_21.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_21.frameNStart = frameN  # exact frame index
            polygon_21.tStart = t  # local t and not account for scr refresh
            polygon_21.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_21, 'tStartRefresh')  # time at next scr refresh
            polygon_21.setAutoDraw(True)
        if polygon_21.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_21.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_21.tStop = t  # not accounting for scr refresh
                polygon_21.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_21, 'tStopRefresh')  # time at next scr refresh
                polygon_21.setAutoDraw(False)
        
        # *polygon_22* updates
        if polygon_22.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_22.frameNStart = frameN  # exact frame index
            polygon_22.tStart = t  # local t and not account for scr refresh
            polygon_22.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_22, 'tStartRefresh')  # time at next scr refresh
            polygon_22.setAutoDraw(True)
        if polygon_22.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_22.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_22.tStop = t  # not accounting for scr refresh
                polygon_22.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_22, 'tStopRefresh')  # time at next scr refresh
                polygon_22.setAutoDraw(False)
        
        # *polygon_23* updates
        if polygon_23.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_23.frameNStart = frameN  # exact frame index
            polygon_23.tStart = t  # local t and not account for scr refresh
            polygon_23.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_23, 'tStartRefresh')  # time at next scr refresh
            polygon_23.setAutoDraw(True)
        if polygon_23.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_23.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_23.tStop = t  # not accounting for scr refresh
                polygon_23.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_23, 'tStopRefresh')  # time at next scr refresh
                polygon_23.setAutoDraw(False)
        
        # *polygon_24* updates
        if polygon_24.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_24.frameNStart = frameN  # exact frame index
            polygon_24.tStart = t  # local t and not account for scr refresh
            polygon_24.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_24, 'tStartRefresh')  # time at next scr refresh
            polygon_24.setAutoDraw(True)
        if polygon_24.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_24.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_24.tStop = t  # not accounting for scr refresh
                polygon_24.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_24, 'tStopRefresh')  # time at next scr refresh
                polygon_24.setAutoDraw(False)
        
        # *polygon_25* updates
        if polygon_25.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_25.frameNStart = frameN  # exact frame index
            polygon_25.tStart = t  # local t and not account for scr refresh
            polygon_25.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_25, 'tStartRefresh')  # time at next scr refresh
            polygon_25.setAutoDraw(True)
        if polygon_25.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_25.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_25.tStop = t  # not accounting for scr refresh
                polygon_25.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_25, 'tStopRefresh')  # time at next scr refresh
                polygon_25.setAutoDraw(False)
        
        # *polygon_26* updates
        if polygon_26.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_26.frameNStart = frameN  # exact frame index
            polygon_26.tStart = t  # local t and not account for scr refresh
            polygon_26.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_26, 'tStartRefresh')  # time at next scr refresh
            polygon_26.setAutoDraw(True)
        if polygon_26.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_26.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_26.tStop = t  # not accounting for scr refresh
                polygon_26.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_26, 'tStopRefresh')  # time at next scr refresh
                polygon_26.setAutoDraw(False)
        
        # *polygon_27* updates
        if polygon_27.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_27.frameNStart = frameN  # exact frame index
            polygon_27.tStart = t  # local t and not account for scr refresh
            polygon_27.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_27, 'tStartRefresh')  # time at next scr refresh
            polygon_27.setAutoDraw(True)
        if polygon_27.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_27.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_27.tStop = t  # not accounting for scr refresh
                polygon_27.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_27, 'tStopRefresh')  # time at next scr refresh
                polygon_27.setAutoDraw(False)
        
        # *polygon_28* updates
        if polygon_28.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_28.frameNStart = frameN  # exact frame index
            polygon_28.tStart = t  # local t and not account for scr refresh
            polygon_28.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_28, 'tStartRefresh')  # time at next scr refresh
            polygon_28.setAutoDraw(True)
        if polygon_28.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_28.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_28.tStop = t  # not accounting for scr refresh
                polygon_28.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_28, 'tStopRefresh')  # time at next scr refresh
                polygon_28.setAutoDraw(False)
        
        # *polygon_29* updates
        if polygon_29.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_29.frameNStart = frameN  # exact frame index
            polygon_29.tStart = t  # local t and not account for scr refresh
            polygon_29.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_29, 'tStartRefresh')  # time at next scr refresh
            polygon_29.setAutoDraw(True)
        if polygon_29.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_29.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_29.tStop = t  # not accounting for scr refresh
                polygon_29.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_29, 'tStopRefresh')  # time at next scr refresh
                polygon_29.setAutoDraw(False)
        
        # *polygon_30* updates
        if polygon_30.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_30.frameNStart = frameN  # exact frame index
            polygon_30.tStart = t  # local t and not account for scr refresh
            polygon_30.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_30, 'tStartRefresh')  # time at next scr refresh
            polygon_30.setAutoDraw(True)
        if polygon_30.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_30.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_30.tStop = t  # not accounting for scr refresh
                polygon_30.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_30, 'tStopRefresh')  # time at next scr refresh
                polygon_30.setAutoDraw(False)
        
        # *polygon_31* updates
        if polygon_31.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_31.frameNStart = frameN  # exact frame index
            polygon_31.tStart = t  # local t and not account for scr refresh
            polygon_31.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_31, 'tStartRefresh')  # time at next scr refresh
            polygon_31.setAutoDraw(True)
        if polygon_31.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_31.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_31.tStop = t  # not accounting for scr refresh
                polygon_31.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_31, 'tStopRefresh')  # time at next scr refresh
                polygon_31.setAutoDraw(False)
        
        # *polygon_32* updates
        if polygon_32.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_32.frameNStart = frameN  # exact frame index
            polygon_32.tStart = t  # local t and not account for scr refresh
            polygon_32.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_32, 'tStartRefresh')  # time at next scr refresh
            polygon_32.setAutoDraw(True)
        if polygon_32.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_32.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_32.tStop = t  # not accounting for scr refresh
                polygon_32.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_32, 'tStopRefresh')  # time at next scr refresh
                polygon_32.setAutoDraw(False)
        
        # *polygon_33* updates
        if polygon_33.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_33.frameNStart = frameN  # exact frame index
            polygon_33.tStart = t  # local t and not account for scr refresh
            polygon_33.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_33, 'tStartRefresh')  # time at next scr refresh
            polygon_33.setAutoDraw(True)
        if polygon_33.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_33.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_33.tStop = t  # not accounting for scr refresh
                polygon_33.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_33, 'tStopRefresh')  # time at next scr refresh
                polygon_33.setAutoDraw(False)
        
        # *polygon_34* updates
        if polygon_34.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_34.frameNStart = frameN  # exact frame index
            polygon_34.tStart = t  # local t and not account for scr refresh
            polygon_34.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_34, 'tStartRefresh')  # time at next scr refresh
            polygon_34.setAutoDraw(True)
        if polygon_34.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_34.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_34.tStop = t  # not accounting for scr refresh
                polygon_34.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_34, 'tStopRefresh')  # time at next scr refresh
                polygon_34.setAutoDraw(False)
        
        # *polygon_35* updates
        if polygon_35.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_35.frameNStart = frameN  # exact frame index
            polygon_35.tStart = t  # local t and not account for scr refresh
            polygon_35.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_35, 'tStartRefresh')  # time at next scr refresh
            polygon_35.setAutoDraw(True)
        if polygon_35.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_35.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_35.tStop = t  # not accounting for scr refresh
                polygon_35.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_35, 'tStopRefresh')  # time at next scr refresh
                polygon_35.setAutoDraw(False)
        
        # *polygon_36* updates
        if polygon_36.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_36.frameNStart = frameN  # exact frame index
            polygon_36.tStart = t  # local t and not account for scr refresh
            polygon_36.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_36, 'tStartRefresh')  # time at next scr refresh
            polygon_36.setAutoDraw(True)
        if polygon_36.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_36.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_36.tStop = t  # not accounting for scr refresh
                polygon_36.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_36, 'tStopRefresh')  # time at next scr refresh
                polygon_36.setAutoDraw(False)
        
        # *polygon_37* updates
        if polygon_37.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_37.frameNStart = frameN  # exact frame index
            polygon_37.tStart = t  # local t and not account for scr refresh
            polygon_37.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_37, 'tStartRefresh')  # time at next scr refresh
            polygon_37.setAutoDraw(True)
        if polygon_37.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_37.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_37.tStop = t  # not accounting for scr refresh
                polygon_37.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_37, 'tStopRefresh')  # time at next scr refresh
                polygon_37.setAutoDraw(False)
        
        # *polygon_38* updates
        if polygon_38.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_38.frameNStart = frameN  # exact frame index
            polygon_38.tStart = t  # local t and not account for scr refresh
            polygon_38.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_38, 'tStartRefresh')  # time at next scr refresh
            polygon_38.setAutoDraw(True)
        if polygon_38.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_38.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_38.tStop = t  # not accounting for scr refresh
                polygon_38.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_38, 'tStopRefresh')  # time at next scr refresh
                polygon_38.setAutoDraw(False)
        
        # *polygon_39* updates
        if polygon_39.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_39.frameNStart = frameN  # exact frame index
            polygon_39.tStart = t  # local t and not account for scr refresh
            polygon_39.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_39, 'tStartRefresh')  # time at next scr refresh
            polygon_39.setAutoDraw(True)
        if polygon_39.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_39.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_39.tStop = t  # not accounting for scr refresh
                polygon_39.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_39, 'tStopRefresh')  # time at next scr refresh
                polygon_39.setAutoDraw(False)
        
        # *polygon_40* updates
        if polygon_40.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            polygon_40.frameNStart = frameN  # exact frame index
            polygon_40.tStart = t  # local t and not account for scr refresh
            polygon_40.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(polygon_40, 'tStartRefresh')  # time at next scr refresh
            polygon_40.setAutoDraw(True)
        if polygon_40.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > polygon_40.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                polygon_40.tStop = t  # not accounting for scr refresh
                polygon_40.frameNStop = frameN  # exact frame index
                win.timeOnFlip(polygon_40, 'tStopRefresh')  # time at next scr refresh
                polygon_40.setAutoDraw(False)
        for clicked_shape in stimuli:
            if mouse.isPressedIn(clicked_shape):
                thisClickTime = clickClock.getTime()
                if (thisClickTime - lastClickTime) > bufferTime:
                    if clicked_shape.fillColor[0] == 1 and clicked_shape.fillColor[1] == -1: # red
                        clicked_shape.color = 'grey'
                        points = points + 1
                        trial_points = trial_points + 1
                    elif clicked_shape.fillColor[1] == 1 and clicked_shape.fillColor[0] == -1: #lime
                        clicked_shape.color = 'grey'
                        points = points + 1
                        trial_points = trial_points + 1
                    else: 
                        distractorClick = 1
                        continueRoutine = False # end current trial
        
        
                lastClickTime = thisClickTime
        
        if trial_points == 20:
            continueRoutine = False # collected everything!
        
        # *mouse* updates
        if mouse.status == NOT_STARTED and t >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            mouse.frameNStart = frameN  # exact frame index
            mouse.tStart = t  # local t and not account for scr refresh
            mouse.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(mouse, 'tStartRefresh')  # time at next scr refresh
            mouse.status = STARTED
            mouse.mouseClock.reset()
            prevButtonState = [0, 0, 0]  # if now button is down we will treat as 'new' click
        if mouse.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > mouse.tStartRefresh + 20-frameTolerance:
                # keep track of stop time/frame for later
                mouse.tStop = t  # not accounting for scr refresh
                mouse.frameNStop = frameN  # exact frame index
                win.timeOnFlip(mouse, 'tStopRefresh')  # time at next scr refresh
                mouse.status = FINISHED
        if mouse.status == STARTED:  # only update if started and not finished!
            buttons = mouse.getPressed()
            if buttons != prevButtonState:  # button state changed?
                prevButtonState = buttons
                if sum(buttons) > 0:  # state changed to a new click
                    # check if the mouse was inside our 'clickable' objects
                    gotValidClick = False
                    try:
                        iter([polygon, polygon_2, polygon_3, polygon_4, polygon_5, polygon_6, polygon_7, polygon_8, polygon_9, polygon_10, polygon_11, polygon_12, polygon_13, polygon_14, polygon_15, polygon_16, polygon_17, polygon_18, polygon_19, polygon_20, polygon_21, polygon_22, polygon_23, polygon_24, polygon_25, polygon_26, polygon_27, polygon_28, polygon_29, polygon_30, polygon_31, polygon_32, polygon_33, polygon_34, polygon_35, polygon_36, polygon_37, polygon_38, polygon_39, polygon_40])
                        clickableList = [polygon, polygon_2, polygon_3, polygon_4, polygon_5, polygon_6, polygon_7, polygon_8, polygon_9, polygon_10, polygon_11, polygon_12, polygon_13, polygon_14, polygon_15, polygon_16, polygon_17, polygon_18, polygon_19, polygon_20, polygon_21, polygon_22, polygon_23, polygon_24, polygon_25, polygon_26, polygon_27, polygon_28, polygon_29, polygon_30, polygon_31, polygon_32, polygon_33, polygon_34, polygon_35, polygon_36, polygon_37, polygon_38, polygon_39, polygon_40]
                    except:
                        clickableList = [[polygon, polygon_2, polygon_3, polygon_4, polygon_5, polygon_6, polygon_7, polygon_8, polygon_9, polygon_10, polygon_11, polygon_12, polygon_13, polygon_14, polygon_15, polygon_16, polygon_17, polygon_18, polygon_19, polygon_20, polygon_21, polygon_22, polygon_23, polygon_24, polygon_25, polygon_26, polygon_27, polygon_28, polygon_29, polygon_30, polygon_31, polygon_32, polygon_33, polygon_34, polygon_35, polygon_36, polygon_37, polygon_38, polygon_39, polygon_40]]
                    for obj in clickableList:
                        if obj.contains(mouse):
                            gotValidClick = True
                            mouse.clicked_name.append(obj.name)
                    x, y = mouse.getPos()
                    mouse.x.append(x)
                    mouse.y.append(y)
                    buttons = mouse.getPressed()
                    mouse.leftButton.append(buttons[0])
                    mouse.midButton.append(buttons[1])
                    mouse.rightButton.append(buttons[2])
                    mouse.time.append(mouse.mouseClock.getTime())
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in trialComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "trial"-------
    for thisComponent in trialComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    trials.addData('polygon.started', polygon.tStartRefresh)
    trials.addData('polygon.stopped', polygon.tStopRefresh)
    trials.addData('polygon_2.started', polygon_2.tStartRefresh)
    trials.addData('polygon_2.stopped', polygon_2.tStopRefresh)
    trials.addData('polygon_3.started', polygon_3.tStartRefresh)
    trials.addData('polygon_3.stopped', polygon_3.tStopRefresh)
    trials.addData('polygon_4.started', polygon_4.tStartRefresh)
    trials.addData('polygon_4.stopped', polygon_4.tStopRefresh)
    trials.addData('polygon_5.started', polygon_5.tStartRefresh)
    trials.addData('polygon_5.stopped', polygon_5.tStopRefresh)
    trials.addData('polygon_6.started', polygon_6.tStartRefresh)
    trials.addData('polygon_6.stopped', polygon_6.tStopRefresh)
    trials.addData('polygon_7.started', polygon_7.tStartRefresh)
    trials.addData('polygon_7.stopped', polygon_7.tStopRefresh)
    trials.addData('polygon_8.started', polygon_8.tStartRefresh)
    trials.addData('polygon_8.stopped', polygon_8.tStopRefresh)
    trials.addData('polygon_9.started', polygon_9.tStartRefresh)
    trials.addData('polygon_9.stopped', polygon_9.tStopRefresh)
    trials.addData('polygon_10.started', polygon_10.tStartRefresh)
    trials.addData('polygon_10.stopped', polygon_10.tStopRefresh)
    trials.addData('polygon_11.started', polygon_11.tStartRefresh)
    trials.addData('polygon_11.stopped', polygon_11.tStopRefresh)
    trials.addData('polygon_12.started', polygon_12.tStartRefresh)
    trials.addData('polygon_12.stopped', polygon_12.tStopRefresh)
    trials.addData('polygon_13.started', polygon_13.tStartRefresh)
    trials.addData('polygon_13.stopped', polygon_13.tStopRefresh)
    trials.addData('polygon_14.started', polygon_14.tStartRefresh)
    trials.addData('polygon_14.stopped', polygon_14.tStopRefresh)
    trials.addData('polygon_15.started', polygon_15.tStartRefresh)
    trials.addData('polygon_15.stopped', polygon_15.tStopRefresh)
    trials.addData('polygon_16.started', polygon_16.tStartRefresh)
    trials.addData('polygon_16.stopped', polygon_16.tStopRefresh)
    trials.addData('polygon_17.started', polygon_17.tStartRefresh)
    trials.addData('polygon_17.stopped', polygon_17.tStopRefresh)
    trials.addData('polygon_18.started', polygon_18.tStartRefresh)
    trials.addData('polygon_18.stopped', polygon_18.tStopRefresh)
    trials.addData('polygon_19.started', polygon_19.tStartRefresh)
    trials.addData('polygon_19.stopped', polygon_19.tStopRefresh)
    trials.addData('polygon_20.started', polygon_20.tStartRefresh)
    trials.addData('polygon_20.stopped', polygon_20.tStopRefresh)
    trials.addData('polygon_21.started', polygon_21.tStartRefresh)
    trials.addData('polygon_21.stopped', polygon_21.tStopRefresh)
    trials.addData('polygon_22.started', polygon_22.tStartRefresh)
    trials.addData('polygon_22.stopped', polygon_22.tStopRefresh)
    trials.addData('polygon_23.started', polygon_23.tStartRefresh)
    trials.addData('polygon_23.stopped', polygon_23.tStopRefresh)
    trials.addData('polygon_24.started', polygon_24.tStartRefresh)
    trials.addData('polygon_24.stopped', polygon_24.tStopRefresh)
    trials.addData('polygon_25.started', polygon_25.tStartRefresh)
    trials.addData('polygon_25.stopped', polygon_25.tStopRefresh)
    trials.addData('polygon_26.started', polygon_26.tStartRefresh)
    trials.addData('polygon_26.stopped', polygon_26.tStopRefresh)
    trials.addData('polygon_27.started', polygon_27.tStartRefresh)
    trials.addData('polygon_27.stopped', polygon_27.tStopRefresh)
    trials.addData('polygon_28.started', polygon_28.tStartRefresh)
    trials.addData('polygon_28.stopped', polygon_28.tStopRefresh)
    trials.addData('polygon_29.started', polygon_29.tStartRefresh)
    trials.addData('polygon_29.stopped', polygon_29.tStopRefresh)
    trials.addData('polygon_30.started', polygon_30.tStartRefresh)
    trials.addData('polygon_30.stopped', polygon_30.tStopRefresh)
    trials.addData('polygon_31.started', polygon_31.tStartRefresh)
    trials.addData('polygon_31.stopped', polygon_31.tStopRefresh)
    trials.addData('polygon_32.started', polygon_32.tStartRefresh)
    trials.addData('polygon_32.stopped', polygon_32.tStopRefresh)
    trials.addData('polygon_33.started', polygon_33.tStartRefresh)
    trials.addData('polygon_33.stopped', polygon_33.tStopRefresh)
    trials.addData('polygon_34.started', polygon_34.tStartRefresh)
    trials.addData('polygon_34.stopped', polygon_34.tStopRefresh)
    trials.addData('polygon_35.started', polygon_35.tStartRefresh)
    trials.addData('polygon_35.stopped', polygon_35.tStopRefresh)
    trials.addData('polygon_36.started', polygon_36.tStartRefresh)
    trials.addData('polygon_36.stopped', polygon_36.tStopRefresh)
    trials.addData('polygon_37.started', polygon_37.tStartRefresh)
    trials.addData('polygon_37.stopped', polygon_37.tStopRefresh)
    trials.addData('polygon_38.started', polygon_38.tStartRefresh)
    trials.addData('polygon_38.stopped', polygon_38.tStopRefresh)
    trials.addData('polygon_39.started', polygon_39.tStartRefresh)
    trials.addData('polygon_39.stopped', polygon_39.tStopRefresh)
    trials.addData('polygon_40.started', polygon_40.tStartRefresh)
    trials.addData('polygon_40.stopped', polygon_40.tStopRefresh)
    if distractorClick == 1:
        myCount = myCount
    else:
        myCount = myCount + 1
                
    print(myCount)
    
    if myCount > 4:
        trials.finished = True
    
    event.clearEvents()
    
    polygon.color = [1, -1, -1]
    polygon_2.color = [1, -1, -1]
    polygon_3.color = [1, -1, -1]
    polygon_4.color = [1, -1, -1]
    polygon_5.color = [1, -1, -1]
    polygon_6.color = [1, -1, -1]
    polygon_7.color = [1, -1, -1]
    polygon_8.color = [1, -1, -1]
    polygon_9.color = [1, -1, -1]
    polygon_10.color = [1, -1, -1]
    polygon_11.color = [1, -1, -1]
    polygon_12.color = [1, -1, -1]
    polygon_13.color = [1, -1, -1]
    polygon_14.color = [1, -1, -1]
    polygon_15.color = [1, -1, -1]
    polygon_16.color = [-1, 1, -1]
    polygon_17.color = [-1, 1, -1]
    polygon_18.color = [-1, 1, -1]
    polygon_19.color = [-1, 1, -1]
    polygon_20.color = [-1, 1, -1]
    polygon_21.color = [-1, -1, 1]
    polygon_22.color = [-1, -1, 1]
    polygon_23.color = [-1, -1, 1]
    polygon_24.color = [-1, -1, 1]
    polygon_25.color = [-1, -1, 1]
    polygon_26.color = [-1, -1, 1]
    polygon_27.color = [-1, -1, 1]
    polygon_28.color = [-1, -1, 1]
    polygon_29.color = [-1, -1, 1]
    polygon_30.color = [-1, -1, 1]
    polygon_31.color = [1, 1, -1]
    polygon_32.color = [1, 1, -1]
    polygon_33.color = [1, 1, -1]
    polygon_34.color = [1, 1, -1]
    polygon_35.color = [1, 1, -1]
    polygon_36.color = [1, 1, -1]
    polygon_37.color = [1, 1, -1]
    polygon_38.color = [1, 1, -1]
    polygon_39.color = [1, 1, -1]
    polygon_40.color = [1, 1, -1]
    
    distractorClick = 0
    
    thisExp.addData("p1", polygon.pos)
    thisExp.addData("p2", polygon_2.pos)
    thisExp.addData("p3", polygon_3.pos)
    thisExp.addData("p4", polygon_4.pos)
    thisExp.addData("p5", polygon_5.pos)
    thisExp.addData("p6", polygon_6.pos)
    thisExp.addData("p7", polygon_7.pos)
    thisExp.addData("p8", polygon_8.pos)
    thisExp.addData("p9", polygon_9.pos)
    thisExp.addData("p10", polygon_10.pos)
    thisExp.addData("p11", polygon_11.pos)
    thisExp.addData("p12", polygon_12.pos)
    thisExp.addData("p13", polygon_13.pos)
    thisExp.addData("p14", polygon_14.pos)
    thisExp.addData("p15", polygon_15.pos)
    thisExp.addData("p16", polygon_16.pos)
    thisExp.addData("p17", polygon_17.pos)
    thisExp.addData("p18", polygon_18.pos)
    thisExp.addData("p19", polygon_19.pos)
    thisExp.addData("p20", polygon_20.pos)
    thisExp.addData("p21", polygon_21.pos)
    thisExp.addData("p22", polygon_22.pos)
    thisExp.addData("p23", polygon_23.pos)
    thisExp.addData("p24", polygon_24.pos)
    thisExp.addData("p25", polygon_25.pos)
    thisExp.addData("p26", polygon_26.pos)
    thisExp.addData("p27", polygon_27.pos)
    thisExp.addData("p28", polygon_28.pos)
    thisExp.addData("p29", polygon_29.pos)
    thisExp.addData("p30", polygon_30.pos)
    thisExp.addData("p31", polygon_31.pos)
    thisExp.addData("p32", polygon_32.pos)
    thisExp.addData("p33", polygon_33.pos)
    thisExp.addData("p34", polygon_34.pos)
    thisExp.addData("p35", polygon_35.pos)
    thisExp.addData("p36", polygon_36.pos)
    thisExp.addData("p37", polygon_37.pos)
    thisExp.addData("p38", polygon_38.pos)
    thisExp.addData("p39", polygon_39.pos)
    thisExp.addData("p40", polygon_40.pos)
    # store data for trials (TrialHandler)
    trials.addData('mouse.x', mouse.x)
    trials.addData('mouse.y', mouse.y)
    trials.addData('mouse.leftButton', mouse.leftButton)
    trials.addData('mouse.midButton', mouse.midButton)
    trials.addData('mouse.rightButton', mouse.rightButton)
    trials.addData('mouse.time', mouse.time)
    trials.addData('mouse.clicked_name', mouse.clicked_name)
    trials.addData('mouse.started', mouse.tStart)
    trials.addData('mouse.stopped', mouse.tStop)
    
    # ------Prepare to start Routine "feedback"-------
    continueRoutine = True
    routineTimer.add(2.000000)
    # update component parameters for each repeat
    feedback_text_2.setText('Your current points total is:')
    feedback_text.setText(points)
    # keep track of which components have finished
    feedbackComponents = [feedback_text_2, feedback_text]
    for thisComponent in feedbackComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    feedbackClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "feedback"-------
    while continueRoutine and routineTimer.getTime() > 0:
        # get current time
        t = feedbackClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=feedbackClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *feedback_text_2* updates
        if feedback_text_2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            feedback_text_2.frameNStart = frameN  # exact frame index
            feedback_text_2.tStart = t  # local t and not account for scr refresh
            feedback_text_2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(feedback_text_2, 'tStartRefresh')  # time at next scr refresh
            feedback_text_2.setAutoDraw(True)
        if feedback_text_2.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > feedback_text_2.tStartRefresh + 2-frameTolerance:
                # keep track of stop time/frame for later
                feedback_text_2.tStop = t  # not accounting for scr refresh
                feedback_text_2.frameNStop = frameN  # exact frame index
                win.timeOnFlip(feedback_text_2, 'tStopRefresh')  # time at next scr refresh
                feedback_text_2.setAutoDraw(False)
        
        # *feedback_text* updates
        if feedback_text.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            feedback_text.frameNStart = frameN  # exact frame index
            feedback_text.tStart = t  # local t and not account for scr refresh
            feedback_text.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(feedback_text, 'tStartRefresh')  # time at next scr refresh
            feedback_text.setAutoDraw(True)
        if feedback_text.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > feedback_text.tStartRefresh + 2-frameTolerance:
                # keep track of stop time/frame for later
                feedback_text.tStop = t  # not accounting for scr refresh
                feedback_text.frameNStop = frameN  # exact frame index
                win.timeOnFlip(feedback_text, 'tStopRefresh')  # time at next scr refresh
                feedback_text.setAutoDraw(False)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in feedbackComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "feedback"-------
    for thisComponent in feedbackComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    trials.addData('feedback_text_2.started', feedback_text_2.tStartRefresh)
    trials.addData('feedback_text_2.stopped', feedback_text_2.tStopRefresh)
    trials.addData('feedback_text.started', feedback_text.tStartRefresh)
    trials.addData('feedback_text.stopped', feedback_text.tStopRefresh)
    thisExp.nextEntry()
    
# completed 1.0 repeats of 'trials'


# Flip one final time so any remaining win.callOnFlip() 
# and win.timeOnFlip() tasks get executed before quitting
win.flip()

# these shouldn't be strictly necessary (should auto-save)
thisExp.saveAsWideText(filename+'.csv', delim='auto')
thisExp.saveAsPickle(filename)
logging.flush()
# make sure everything is closed down
thisExp.abort()  # or data files will save again on exit
win.close()
core.quit()
