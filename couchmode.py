import random
from collections import namedtuple, deque
import hy
from org_drill import (
        determine_next_interval_sm2,
        determine_next_interval_sm5,
        determine_next_interval_simple8,
        org_drill_failure_quality
        )


class AbstractLearningItem():

    NextIntervalTuple = namedtuple(
        'NextIntervalTuple',
        ['lastInterval', 'n', 'ease', 'failures', 'mq', 'total'])

    def __init__(
            self, lastInterval=-1, n=0, ease=None, failures=0, mq=0, total=0):
        self.lastInterval = lastInterval
        self.n = n
        self.ease = ease
        self.failures = failures
        self.mq = mq
        self.total = total

    def learn(self, quality):
        failed = quality <= org_drill_failure_quality
        result = self._getNextInterval(quality)
        self.lastInterval = result.lastInterval
        if failed:
            self.failures += 1
        else:
            self.n += 1
        self.ease = result.ease
        self.mq = result.mq
        self.total += 1
        return result

    def _getNextInterval(self, quality):
        raise NotImplementedError


class LearningItemSM2(AbstractLearningItem):

    def _getNextInterval(self, quality):
        return self.NextIntervalTuple._make(determine_next_interval_sm2(
            self.lastInterval, self.n, self.ease, quality, self.failures,
            self.mq, self.total))


class LearningItemSM5(AbstractLearningItem):

    def __init__(
            self, lastInterval=-1, n=0, ease=None, failures=0, mq=0, total=0,
            ofMatrix=None):
        AbstractLearningItem.__init__(
            self, lastInterval=lastInterval, n=n, ease=ease, failures=failures,
            mq=mq, total=total)
        self.ofMatrix = ofMatrix

    def _getNextInterval(self, quality):
        rawResult = determine_next_interval_sm5(
            self.lastInterval, self.n, self.ease, quality, self.failures,
            self.mq, self.total, self.ofMatrix)
        ofPos = len(self.NextIntervalTuple._fields)
        self.ofMatrix = rawResult[ofPos:]
        return self.NextIntervalTuple._make(rawResult[:ofPos])


class LearningItemSimple8(AbstractLearningItem):

    def __init__(self, lastInterval=-1, n=0, failures=0, mq=0, total=0):
        AbstractLearningItem.__init__(
            self, lastInterval=lastInterval, n=n, failures=failures, mq=mq,
            total=total)

    def _getNextInterval(self, quality):
        return self.NextIntervalTuple._make(determine_next_interval_simple8(
            self.lastInterval, self.n, quality, self.failures, self.mq,
            self.total))


def main():
    # example
    keys = {
        'False', 'None', 'True', 'and', 'as', 'assert', 'break', 'class',
        'continue', 'def', 'del', 'elif', 'else', 'except', 'finally', 'for',
        'from', 'global',
    }
    deck = {dk: LearningItemSM2() for dk in keys}
    # initialize
    for dk in deck:
        deck[dk].learn(0)
    recentDK = deque(maxlen=2)  # memorized with quality 5
    boringDK = deque(maxlen=2)  # will be memorized
    for i in range(50):
        if len(recentDK) > 0:
            choices = keys - {recentDK[-1]}
        else:
            choices = keys
        minInterval = deck[min(
            choices, key=lambda dk: deck[dk].lastInterval)].lastInterval
        dk = random.choice(list(filter(
            lambda dk: deck[dk].lastInterval == minInterval,
            choices)))
        if dk in recentDK:
            quality = 5
        elif dk in boringDK:
            quality = random.randint(org_drill_failure_quality + 1, 5)
        else:
            quality = random.randint(0, 5)
        ret = deck[dk].learn(quality)
        print('"{}" [{:g}, {:g}, {:g}]'.format(
            dk, ret.lastInterval, ret.ease, ret.mq))
        if dk in boringDK:
            boringDK.remove(dk)
        if 0 < len(recentDK):
            if dk in recentDK:
                recentDK.remove(dk)
            else:
                boringDK.append(recentDK.pop())
        recentDK.append(dk)


if __name__ == '__main__':
    main()
